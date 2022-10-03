(** {1 Dune files}

    {b dune} files are the main part of Dune. They are used to describe libraries, executables, tests, and everything Dune needs to know about.

    The official documentation is {{:https://dune.readthedocs.io/en/stable/dune-files.html#dune}on the Dune website}.
*)

(** The module type for an embedded domain specific language (eDSL) that describes a Dune file. *)
module type SYM = sig
  type 'a repr
  (** A type that represents a Dune file. *)

  (** {2 Stanzas} *)

  val rule : [ `RuleClause ] repr list -> [ `Stanza ] repr
  (** The [rule [rule_clause1 ...]] stanza is used to create custom user rules. It tells Dune how to generate a specific set of files from a specific set of dependencies. *)

  val executable : [ `Executable ] repr list -> [ `Stanza ] repr

  val install : [ `Install ] repr list -> [ `Stanza ] repr

  (** {3 Rules} *)

  val alias : string -> [ `RuleClause ] repr

  val targets : string list -> [ `RuleClause ] repr

  val target : string -> [ `RuleClause ] repr

  val deps : [ `Dep ] repr list -> [ `RuleClause ] repr

  val action : [ `Action ] repr -> [ `RuleClause ] repr
  (** [action <action>] is what you run to produce the targets from the dependencies. *)

  (** {4 Dependencies} *)

  val glob_files : string -> [ `Dep ] repr

  val named_dep : name:string -> string -> [ `Dep ] repr

  (** {4 Actions} *)

  val echo : string list -> [ `Action ] repr

  val with_stdout_to : string -> [ `Action ] repr -> [ `Action ] repr

  val progn : [ `Action ] repr list -> [ `Action ] repr

  val run : string list -> [ `Action ] repr

  val diff : actual:string -> expected:string -> [ `Action ] repr

  val diff_q : actual:string -> expected:string -> [ `Action ] repr

  (** {3 Executables} *)

  val public_name : string -> [ `Executable ] repr

  val name : string -> [ `Executable ] repr

  val libraries : string list -> [ `Executable ] repr

  val modules : string list -> [ `Executable ] repr

  val modes_byte_exe : [ `Executable ] repr

  val ocamlopt_flags : [ `OCamlOptFlag ] repr list -> [ `Executable ] repr

  (** {3 Install} *)

  val section : string -> [ `Install ] repr

  val install_files : [ `InstallDestination ] repr list -> [ `Install ] repr
  (** {{:https://dune.readthedocs.io/en/stable/dune-files.html#install-1}}.

      Can either be:

      {v
      (files   (mylib.el as emacs/site-lisp/mylib.el))
      v}

      or

      {v
      (files hello.txt)
      v}
    *)

  (** {4 Install Destination} *)

  val destination_file :
    filename:string -> destination:string -> [ `InstallDestination ] repr
  (** {{:https://dune.readthedocs.io/en/stable/dune-files.html#install-1}}.

      Represents:

      {v
      (mylib.el as emacs/site-lisp/mylib.el)
      v}

      in

      {v
        (files   (mylib.el as emacs/site-lisp/mylib.el))
      v}
   *)
end
