open DkmlDuneDslShow
open Sexplib

let args = { params = `O []; params_idx = 0; entire_params_file = `O [] }

let check_repr =
  let pp fmt (repr : args -> out) =
    let out = repr args in
    let out_plain_sexp = Option.map Sexp.With_layout.Forget.t out in
    Fmt.(option Sexplib0.Sexp.pp_mach) fmt out_plain_sexp
  in
  Alcotest.testable pp (fun a b ->
      let a_out = a args and b_out = b args in
      match (a_out, b_out) with
      | None, Some _ | Some _, None -> false
      | None, None -> true
      | Some v1, Some v2 ->
          Sexp.equal
            (Sexp.With_layout.sexp_of_t v1)
            (Sexp.With_layout.sexp_of_t v2))

let repr_equals = Alcotest.check check_repr "equals"

let zero_pos = { Sexp.With_layout.row = 0; col = 0 }

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
          ( "no items",
            `Quick,
            fun () -> repr_equals (fun _args -> None) (I.set_of []) );
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
          ( "no items",
            `Quick,
            fun () -> repr_equals (layout_sexp ":standard") I.standard );
        ] );
      ( "split",
        [
          ( "no items",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "(App_p Section_p)")
                (I.split " App_p  Section_p ") );
        ] );
    ]
