(** assert_file_contains.exe FILE EXPECT checks the __first__ line of the FILE and compares it to EXPECT. *)

let () =
  let filename = Sys.argv.(1) in
  let expected = Sys.argv.(2) in
  let ic = open_in filename in
  let line = input_line ic in
  assert (line = expected);
  close_in ic
