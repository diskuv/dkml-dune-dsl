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

  val executable : [ `CommonExecutableLibrary ] repr list -> [ `Stanza ] repr
  (** The [executable [name "<name>"; ...]] stanza must be used to describe an executable.
      
      ["<name>"] is a module name that contains the executable’s main entry point. There can be additional
      modules in the current directory; you only need to specify the entry point. Given an executable
      stanza with [name "<name>"; ...], Dune will know how to build ["<name>.exe"]. If requested, it will
      also know how to build ["<name>.bc"] and ["<name>.bc.js"]. Dune 2.0 and up also need specific configuration
      (see the {!modes} optional field below).
  *)

  val library : [ `CommonExecutableLibrary ] repr list -> [ `Stanza ] repr
  (** The [library [name "<library-name>"; ...]] stanza must be used to describe OCaml libraries.
  
      ["<library-name>"] is the real name of the library. It determines the names of the archive files
      generated for the library as well as the module name under which the library will be available,
      unless [wrapped false] is used (see {!wrapped}). It must be a valid OCaml module name, but it doesn’t
      need to start with an uppercase letter.
  *)

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

  (** {3 Executables and Libraries} *)

  val public_name : string -> [ `CommonExecutableLibrary ] repr

  val name : string -> [ `CommonExecutableLibrary ] repr

  val libraries : string list -> [ `CommonExecutableLibrary ] repr

  val modules : string list -> [ `CommonExecutableLibrary ] repr

  val modes_byte_exe : [ `CommonExecutableLibrary ] repr

  val ocamlopt_flags :
    [ `OCamlOptFlag ] repr list -> [ `CommonExecutableLibrary ] repr

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
