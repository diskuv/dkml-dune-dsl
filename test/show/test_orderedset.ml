open DkmlDuneDslShow
open Testutils

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
                (layout_sexp
                   "(modules (:standard \\ :standard) a ((:standard \\ \
                    :standard) b c))")
                I.(
                  modules
                    (union
                       [
                         union [ union [ union [ set_of [ "a" ] ] ] ];
                         set_of [ "b"; "c" ];
                       ])) );
        ] );
    ]
