module Dune = Dune
(** {b dune} files are the main part of Dune. They are used to describe libraries, executables, tests, and everything Dune needs to know about. *)

(**/**)

let spec_version = 1

let spec_modules =
  [
    ("Build", "Dune"); ("Project", "DuneProject"); ("Workspace", "DuneWorkspace");
  ]
