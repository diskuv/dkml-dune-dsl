open DkmlDuneDslShow
open Testutils

let () =
  let open Alcotest in
  run "`BooleanLanguage"
    [
      ( "bool",
        [
          ( "constant",
            `Quick,
            fun () -> repr_equals (layout_sexp "macos") (I.template "macos") );
          ( "variable",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "%{ocaml-config:system}")
                (I.template "%{ocaml-config:system}") );
        ] );
      ( "not",
        [
          ( "constant",
            `Quick,
            fun () ->
              repr_equals (layout_sexp "(not true)") I.(not (template "true"))
          );
        ] );
      ( "op",
        [
          ( "=",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "(= true false)")
                I.(eq (template "true") (template "false")) );
          ( "<>",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "(<> true false)")
                I.(ne (template "true") (template "false")) );
        ] );
      ( "or",
        [
          ( "no items",
            `Quick,
            fun () ->
              check_raises "fails validation"
                (Failure "(or) must contain at least one boolean expression")
                (fun () -> repr_equals (layout_sexp "macos") (I.any [])) );
          ( "one item",
            `Quick,
            fun () ->
              repr_equals (layout_sexp "(or true)")
                (I.any I.[ template "true" ]) );
          ( "two items",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "(or true false)")
                (I.any I.[ template "true"; template "false" ]) );
        ] );
      ( "and",
        [
          ( "no items",
            `Quick,
            fun () ->
              check_raises "fails validation"
                (Failure "(all) must contain at least one boolean expression")
                (fun () -> repr_equals (layout_sexp "macos") (I.all [])) );
          ( "one item",
            `Quick,
            fun () ->
              repr_equals (layout_sexp "(and true)")
                (I.all I.[ template "true" ]) );
          ( "two items",
            `Quick,
            fun () ->
              repr_equals
                (layout_sexp "(and true false)")
                (I.all I.[ template "true"; template "false" ]) );
        ] );
    ]
