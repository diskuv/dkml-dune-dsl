open DkmlDuneDslShow
open Sexplib

let mock_args =
  {
    params = `O [ ("test-param-string", `String "testvalue") ];
    params_idx = 0;
    entire_params_file = `O [];
  }

let check_repr =
  let pp fmt (repr : args -> out) =
    let out = repr mock_args in
    let out_plain_sexp = Option.map Sexp.With_layout.Forget.t out in
    Fmt.(option Sexplib0.Sexp.pp_mach) fmt out_plain_sexp
  in
  Alcotest.testable pp (fun a b ->
      let a_out = a mock_args and b_out = b mock_args in
      match (a_out, b_out) with
      | None, Some _ | Some _, None -> false
      | None, None -> true
      | Some v1, Some v2 ->
          Sexp.equal
            (Sexp.With_layout.sexp_of_t v1)
            (Sexp.With_layout.sexp_of_t v2))

let repr_equals = Alcotest.check check_repr "equals"

let zero_pos = { Sexp.With_layout.row = 0; col = 0 }

let no_sexp _args = None

let rec sexp_to_sexp_with_layout = function
  | Sexp.Atom a -> Sexp.With_layout.Atom (zero_pos, a, None)
  | Sexp.List l ->
      Sexp.With_layout.List
        ( zero_pos,
          List.map
            (fun x -> Sexp.With_layout.Sexp (sexp_to_sexp_with_layout x))
            l,
          zero_pos )

let layout_sexp s _args : Sexp.With_layout.t option =
  let v = Sexp.of_string s in
  Some (sexp_to_sexp_with_layout v)

let () =
  let open Alcotest in
  run "`OrderedSet"
    [
      ( "set_of",
        [
          ("no items", `Quick, fun () -> repr_equals no_sexp (I.set_of []));
          ( "one item",
            `Quick,
            fun () -> repr_equals (layout_sexp "(hi)") (I.set_of [ "hi" ]) );
          ( "two items",
            `Quick,
            fun () ->
              repr_equals (layout_sexp "(hi there)")
                (I.set_of [ "hi"; "there" ]) );
        ] );
      ( "standard",
        [
          ( ":standard",
            `Quick,
            fun () -> repr_equals (layout_sexp ":standard") I.standard );
        ] );
      ( "split",
        [
          ( "expected failure: unclosed Mustache loop",
            `Quick,
            fun () ->
              let raised =
                try
                  let (_ : out) = I.split "{{#test-param-string}}" mock_args in
                  false
                with _e -> true
              in
              if not raised then
                fail "Expected but did not get Mustache failure" );
          ( "split after params evaluated",
            `Quick,
            fun () ->
              repr_equals no_sexp
                (* this is a regression test. if splitting happens before
                   params are evaluated, then you will get
                   ["{{#test-param-string}}"; "{{/test-param-string}}"] atoms,
                   and those atoms will fail parameter evaluation. We
                   tested the unclosed Mustache loop atom in the previous test
                   case to make sure it fails!

                   Only if split is done after params can this be valid. *)
                (I.split "{{#test-param-string}} {{/test-param-string}}") );
          ( "two items",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "(App_p Section_p)")
                (I.split " App_p  Section_p ") );
        ] );
      ( "no-first-atom",
        (* Dune on https://dune.readthedocs.io/en/stable/concepts.html#ordered-set-language says:

            > Note that inside an ordered set, the first element of a list cannot be an atom except
            > if it starts with - or :.

           So we test an expression that uses the ordered set (modules ...) which exercises
           the _arg_of_ordset logic that promotes single element lists into atoms.
        *)
        [
          ( "((((a)))) (b c))",
            `Quick,
            fun () ->
              repr_equals
                (* Should NOT be: (modules ((((a)))) (b c)) *)
                (layout_sexp "(modules a (b c))")
                I.(
                  modules
                    (union
                       [
                         union [ union [ union [ set_of [ "a" ] ] ] ];
                         set_of [ "b"; "c" ];
                       ])) );
        ] );
    ]
