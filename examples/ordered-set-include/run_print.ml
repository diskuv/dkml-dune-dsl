(* Because [run_print] is part of [Lib_a.A], the [run_print] function call
   below won't work unless there is [-open Lib_a.A] options have been
   given to ocamlc and ocamlopt. *)
let () = run_print ()
