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
    let out_plain_sexp = List.map Sexp.With_layout.Forget.t out in
    Fmt.(list Sexplib0.Sexp.pp_mach) fmt out_plain_sexp
  in
  Alcotest.testable pp (fun a b ->
      let a_out = a mock_args and b_out = b mock_args in
      match (a_out, b_out) with
      | [], [] -> true
      | [ v1 ], [ v2 ] ->
          Sexp.equal
            (Sexp.With_layout.sexp_of_t v1)
            (Sexp.With_layout.sexp_of_t v2)
      | _ -> false)

let repr_equals = Alcotest.check check_repr "equals"
let zero_pos = { Sexp.With_layout.row = 0; col = 0 }
let no_sexp _args = []

let rec sexp_to_sexp_with_layout = function
  | Sexp.Atom a -> Sexp.With_layout.Atom (zero_pos, a, None)
  | Sexp.List l ->
      Sexp.With_layout.List
        ( zero_pos,
          List.map
            (fun x -> Sexp.With_layout.Sexp (sexp_to_sexp_with_layout x))
            l,
          zero_pos )

let layout_sexp s _args : Sexp.With_layout.t list =
  let v = Sexp.of_string s in
  [ sexp_to_sexp_with_layout v ]
