(** assert_file_contains.exe FILE EXPECT checks the __first__ line of the FILE and compares it to EXPECT. *)

let () =
  let filename = Sys.argv.(1) in
  let expected = Sys.argv.(2) in
  let ic = open_in filename in
  let line = input_line ic in
  if not (String.equal line expected) then (
    (* Show underscores to help with trailing or leading blanks *)
    let underscores = String.map (fun _c -> '_') line in
    let msg =
      Printf.sprintf
        "The first line of %s did not match what was expected!\n\
         %s\n\
         Expected:\n\
         %s\n\
         %s\n\
         Actual:\n\
         %s\n\
         %s\n"
        filename underscores expected underscores line underscores
    in
    prerr_endline ("FATAL: " ^ msg);
    failwith msg);
  close_in ic
