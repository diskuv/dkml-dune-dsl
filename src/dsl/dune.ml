(** {1 Dune files}

    {b dune} files are the main part of Dune. They are used to describe libraries, executables, tests, and everything Dune needs to know about.

    The official documentation is {{:https://dune.readthedocs.io/en/stable/dune-files.html#dune}on the Dune website}.

    {2 Argument Handling}

    Many Dune expressions have a list of strings as arguments. For example, the {!SYM.libraries} expression declares the list of
    libraries that an executable needs. Use one of the following variants to tell the expression interpreter how to
    process your string argument:

    - [`S "something"] is a literal string
    - [`Split "something1 something2 ..."] is zero or more strings split by whitespace (roughly speaking). The formal
      splitting algorithm follows the
      {{:https://github.com/janestreet/sexplib#lexical-conventions-of-s-expression}Lexical conventions of s-expression}
      for identifying atoms in a list.

    All the expressions below are equivalent:

    [
      (libraries [`S "lib1"; `S "lib2"; `S "lib3"])
      (libraries [`Split "lib1 lib2 lib3"])
      (libraries [`S "lib1"; `Split "lib2 lib3"])
    ]

    Why use [`Split]? Besides saving a bit of typing for long lists, a [`Split] is the right choice when your interpreter
    supports parameterized strings. For example, if you were using the ["dkml-dune-dsl-show"] interpreter and configured
    it with a JSON parameter file, you could use [`Split "{{#libraries}} {{library}} {{/libraries}}"] and
    the ["dkml-dune-dsl-show"] interpreter would:
    
    + concatenate all of the [libraries] JSON array from the JSON parameter file into a space separated list of libraries
    + [`Split] would then split the space separated list of libraries into the individual libraries.
*)

(** The module type for an embedded domain specific language (eDSL) that describes a Dune file. *)
module type SYM = sig
  type 'a repr
  (** A type that represents a Dune file. *)

  (** {2 Stanzas} *)

  val rule : [ `RuleClause ] repr list -> [ `Stanza ] repr
  (** The [rule [rule_clause1 ...]] stanza is used to create custom user rules. It tells Dune how to generate a specific set of files from a specific set of dependencies. *)

  val executable : [ `Executable ] repr list -> [ `Stanza ] repr
  (** The [executable [name "<name>"; ...]] stanza must be used to describe an executable.

      ["<name>"] is a module name that contains the executable's main entry point. There can be additional
      modules in the current directory; you only need to specify the entry point. Given an executable
      stanza with [name "<name>"; ...], Dune will know how to build ["<name>.exe"]. If requested, it will
      also know how to build ["<name>.bc"] and ["<name>.bc.js"]. Dune 2.0 and up also need specific configuration
      (see the {!modes} optional field below).
  *)

  val library : [ `Library ] repr list -> [ `Stanza ] repr
  (** The [library [name "<library-name>"; ...]] stanza must be used to describe OCaml libraries.

      ["<library-name>"] is the real name of the library. It determines the names of the archive files
      generated for the library as well as the module name under which the library will be available,
      unless [wrapped false] is used (see {!wrapped}). It must be a valid OCaml module name, but it doesn't
      need to start with an uppercase letter.
  *)

  val install : [ `Install ] repr list -> [ `Stanza ] repr
  (** [install] copies freshly built artifacts from the workspace to the system.
      
      The install stanza takes three pieces of information:

      + the list of files to install
      + the package to attach these files. (This field is optional if your project contains a single package.)
      + the {!section} in which the files will be installed

      See {!section-Install} for how to specify the three pieces of information. 
    *)

  val pragma : string -> [ `Stanza ] repr -> [ `Stanza ] repr
  (** [pragma instruction stanza] gives an [instruction] to the interpreter that modifies the
      behavior of a Dune [stanza].

      The standard pragmas are:

      + ["once"] - Use this for the {b dkml-dune-dsl-show} interpreter, and any other
        compliant interpreter, to indicate that the [stanza] should be included at most once.
        Often you will have common build rules that should not be include multiple times
        simply because you have multiple parameter sets.

        For the {b dkml-dune-dsl-show} interpreter, a tell-tale sign that you should
        use ["once"] is when your rules do not have any parameters ["{{ }}"] in them.
      *)

  (** {3 Rules} *)

  val alias : string -> [ `RuleClause ] repr
  (** [alias name] specifies the rule’s alias is [name]. Building the alias [name] means building
      the targets of this rule and all other rules that share the [name]. *)

  val targets :
    [< `S of string | `Split of string ] list -> [ `RuleClause ] repr
  (** [targets filenames] is a list of [filenames] that will be produced by the rule. *)

  val target : string -> [ `RuleClause ] repr
  (** [target filename] is the [filename] that will be produced by the rule. *)

  val deps : [ `Dep ] repr list -> [ `RuleClause ] repr
  (** [deps [dep1; ...]] declares dependencies [[dep1; ...]] required by the rule.

      See {!section-Dependencies} for which dependencies are allowed. *)

  val action : [ `Action ] repr -> [ `RuleClause ] repr
  (** [action <action>] is what you run to produce the targets from the dependencies. *)

  type mode = Standard | Fallback | Promote

  val mode : mode -> [ `RuleClause ] repr
  (** [mode mode] can change the default ({!Standard}) behavior when a source file exists
      with the same name as the rule {!target}.

      - [Standard] - the standard mode.
      - [Fallback] - in this mode, when the targets are already present in the source tree,
        Dune will ignore the rule. It's an error if only a subset of the targets are
        present in the tree. Fallback rules are commonly used to generate default
        configuration files that may be generated by a configure script.
      - [Promote] - in this mode, the files in the source tree will be ignored. Once the
        rule has been executed, the targets will be copied back to the source tree. *)

  (** {4:Dependencies Dependencies} *)

  val glob_files : string -> [ `Dep ] repr
  (** [glob_files glob] depends on all files matched by [glob].
      
      You can use globs to declare dependencies on a set of files. Note that globs will
      match files that exist in the source tree as well as buildable targets, so for instance you
      can depend on ["*.cmi"].
      
      The glob syntax is interpreted as follows:

      - ["\\<char>"] matches exactly ["<char>"], even if it’s a special character (["*"], ["?"], ...).
      - ["*"] matches any sequence of characters, except if it comes first, in which case it matches any character that is not . followed by anything.
      - ["**"] matches any character that is not . followed by anything, except if it comes first, in which case it matches anything.
      - ["?"] matches any single character.
      - ["[<set>]"] matches any character that is part of ["<set>"].
      - ["[!<set>]"] matches any character that is not part of ["<set>"].
      - ["{<glob1>,<glob2>,...,<globn>}"] matches any string that is matched by one of ["<glob1>"], ["<glob2>"], etc.
      *)

  val named_dep : name:string -> string -> [ `Dep ] repr

  (** {4 Actions} *)

  val echo : string list -> [ `Action ] repr
  (** [echo [s1; s2; ...]] writes the strings [s1; s2; ...] to the standard output
      separated by a space. *)

  val with_stdout_to : string -> [ `Action ] repr -> [ `Action ] repr
  (** [with_stdout_to filename action] redirects the output of action [action]
      to the file named [filename]. *)

  val progn : [ `Action ] repr list -> [ `Action ] repr
  (** [progn [action1; action2; ...]] executes several actions [action1; ...] in sequence *)

  val copy : src:string -> dest:string -> [ `Action ] repr
  (** [copy ~src ~dest] copies the [src] file to [dest] *)

  val run : [< `S of string | `Split of string ] list -> [ `Action ] repr
  (** [run [`S prog; `S arg1; `S arg2; ...]] runs the program [prog] and gives it arguments [arg1; arg2; ...]. *)

  val diff : actual:string -> expected:string -> [ `Action ] repr
  (** [diff ~actual ~expected] is similar to [run ["diff"; "<actual>"; "<expected>"]] but is
      better and allows promotion. See
      {{:https://dune.readthedocs.io/en/stable/concepts.html#diffing-and-promotion}Diffing and promotion} for more details. *)

  val diff_q : actual:string -> expected:string -> [ `Action ] repr
  (** [diff_q ~actual ~expected] is the ["diff?"] action in a "dune" file, and is similar to
      [run ["diff"; "<actual>"; "<expected>"]] except that
      ["<expected>"] should be produced by a part of the same action rather than be a dependency, is optional
      and will be consumed by [diff_q]. See
      {{:https://dune.readthedocs.io/en/stable/concepts.html#diffing-and-promotion}Diffing and promotion} for more details. *)

  val setenv :
    name:string -> value:string -> [ `Action ] repr -> [ `Action ] repr
  (** [setenv ~name ~value action] sets the environment variable [name] to [value]
      in the action [action] *)

  (** {3 Executables and Libraries} *)

  (** This section has expressions that work in both {!executable} and {!library} stanzas. *)

  val public_name : string -> [< `Executable | `Library ] repr
  (** [public_name] is the name under which the library can be referred as a dependency when it is installed outside of
      the current workspace, or is the name of the executable when it is installed outside of the current workspace.
      
      {b [public_name] for Libraries}

      Without a [(public_name ...)] field the library won’t be installed by Dune. The public name
      must start with the package name it’s part of and optionally followed by a dot, then anything else you want.

      {b [public_name] for Executables}

      Without a [(public_name ...)] field the executable won’t be installed by Dune. 
    *)

  val name : string -> [< `Executable | `Library ] repr
  (** [name] is the module name that contains the executable’s main entry point, or the real name of the library.
  
      {b [name] for Libraries}
      
      It determines the names of the archive files generated for the library as well as the module name under
      which the library will be available, unless [(wrapped false)] is used (see {!wrapped}). It must be a
      valid OCaml module name, but it doesn’t need to start with an uppercase letter.

      {b [name] for Executables}

      There can be additional modules in the current directory; you only need to specify the entry point.
      Given an {!executable} stanza with [(name <name>)], Dune will know how to build ["<name>.exe"].

*)

  val libraries :
    [< `S of string | `Split of string ] list ->
    [< `Executable | `Library ] repr
  (** [libraries [`S lib1; `S lib2; ...]] specifies the library's dependencies *)

  val modules : [ `OrderedSet ] repr -> [< `Executable | `Library ] repr
  (** [modules ordered_set] specifies what modules are part of the library.

      By default, Dune will use all the .ml/.re files in the same directory as the dune file. This includes ones
      present in the file system as well as ones generated by user rules. You can restrict this list by using a
      [modules ordered_set] field. [modules] uses the {!section-"Ordered-Sets"} Language, where elements are module
      names and don't need to start with an uppercase letter. For instance, to exclude module [Foo], use
      [modules (difference (standard) (set_of ["foo"]))]; in a real Dune file you would write that same expression
      as ["(modules (:standard \ foo))"]

      See {!section-"Ordered-Sets"} for the operations you can perform.
    *)

  type compilation_mode =
    | Byte
    | Native
    | Best
        (** [compilation_mode] must be byte, native, or best, where best is native with a fallback to bytecode when
      native compilation isn't available. *)

  type binary_kind =
    | C
    | Exe
    | Object
    | Shared_object
    | Js
    | Plugin
        (** [binary_kind] is one of:

      - [C] for producing OCaml bytecode embedded in a C file
      - [Exe] for normal executables
      - [Object] for producing static object files that can be manually linked into C applications
      - [Shared_object] for producing object files that can be dynamically loaded into an application.
        This mode can be used to write a plugin in OCaml for a non-OCaml application.
      - [Js] for producing JavaScript from bytecode executables, see explicit_js_mode.
      - [Plugin] for producing a plugin (.cmxs if native or .cma if bytecode). *)

  val modes :
    [< `C
    | `Exe
    | `Object
    | `Shared_object
    | `Byte
    | `Native
    | `Js
    | `Plugin
    | `Byte_complete
    | `Mode of compilation_mode * binary_kind ]
    list ->
    [< `Executable | `Library ] repr
  (** The [modes] field allows selecting which linking modes will be used to link executables. Each mode is a
      pair (<compilation-mode> <binary-kind>), where <compilation-mode> describes whether the bytecode or
      native code backend of the OCaml compiler should be used and <binary-kind> describes what kind of file
      should be produced.

      Refer to {!compilation_mode} and {!binary_kind} for precise semantics.

      For instance the following executables stanza will produce bytecode executables and native shared objects:

      {v
(executable
  (name "a")
  (modes [`Mode (Byte, Exe); `Mode (Native; Shared_object)]))
      v}

      Additionally, you can use the following shorthands:

      - `C for (byte c)
      - `Exe for (best exe)
      - `Object for (best object)
      - `Shared_object for (best shared_object)
      - `Byte for (byte exe)
      - `Native for (native exe)
      - `Js for (byte js)
      - `Plugin for (best plugin)

      Lastly, use the special mode [`Byte_complete] for building a bytecode executable as a native
      self-contained executable, i.e., an executable that doesn't require the ocamlrun program to
      run and doesn't require the C stubs to be installed as shared object files.
  *)

  val ocamlopt_flags :
    [ `OCamlOptFlag ] repr list -> [< `Executable | `Library ] repr

  val wrapped : bool -> [< `Executable | `Library ] repr
  (** [wrapped false] or [wrapped true] specifies whether the library modules should be available only
      through the top-level library module, or if they should all be exposed at the top level.

      The default is [wrapped true], and it's highly recommended to keep it this way. Because OCaml
      top-level modules must all be unique when linking an executables, polluting the top-level namespace
      will make your library unusable with other libraries if there is a module name clash. This option
      is only intended for libraries that manually prefix all their modules by the library name and to
      ease porting of existing projects to Dune. *)

  val preprocess : [ `PreprocessSpec ] repr -> [< `Executable | `Library ] repr
  (** [preprocess spec] specifies how to preprocess files when needed. The default
      is {!no_preprocessing}.

      The full list of specifications is in {!section-Preprocessing}. *)

  (** {4:Preprocessing Preprocessing} *)

  val no_preprocessing : [ `PreprocessSpec ] repr
  (** [no_preprocessing] tells Dune to give files as-is to the compiler *)

  val pps :
    [< `S of string | `Split of string ] list -> [ `PreprocessSpec ] repr
  (** [pps [`S ppx1; `S ppx2; ...]] preprocesses files using the given list of PPX rewriters *)

  val staged_pps :
    [< `S of string | `Split of string ] list -> [ `PreprocessSpec ] repr
  (** [staged_pps [`S ppx1; `S ppx2; ...]] preprocesses files using the given list of PPX rewriters
      {b after} dependency analysis.

      It is slower than {!pps}, but you must use [staged_pps] instead of [pps] in order to force
      Dune to use the following pipeline:

      + first stage of code generation
      + dependency analysis
      + second step of code generation in parallel with compilation
    *)

  val future_syntax : [ `PreprocessSpec ] repr
  (** [future_syntax] is equivalent to {!no_preprocessing} when using one of the most recent
      versions of the compiler. When using an older one, it is a shim preprocessor that backports
      some of the newer syntax elements. This allows you to use some of the new OCaml features
      while keeping compatibility with older compilers.
    *)

  (** {4:Ordered-Sets Ordered Sets}

      For fields that can take multiple arguments, an [[`OrderedSet] repr] lets you specify your arguments using
      set operations. You can:
      - list each argument out individually with {!set_of}
      - {!split} a string into separate arguments
      - use the {!standard} arguments
      - take the {!union} (also known as concatenation) of any other set
      - take the {!difference} between two sets

      The authorative reference is {{:https://dune.readthedocs.io/en/stable/concepts.html#ordered-set-language-1}Ordered Set Language}.

      Here are some common sets to get your started:

      - [set_of ["a"; "b"; "c"]] are the set of arguments ["a"], ["b"] and ["c"]
      - [split "a b c"] are the set of arguments ["a"], ["b"] and ["c"]
      - [split "{{#param-sets}} {{{ module }}} {{/param-sets}}"] are the set of arguments after calculating the Mustache expression.
        The ["param-sets"] field of your parameter file will be available if you use the [pragma "once" ...]; see {!pragma}. With
        the example Mustache expression, Mustache will collect all of the [module] fields of your parameter file into a single string,
        and then [split] will split those modules by atoms.
      - [(difference (standard) (set_of ["foo"]))] which is all the standard arguments (ex. standard modules) except for ["foo"]. In a real
        Dune file would be written as ["(:standard \ foo)"]

      It is allowed grammatically but is highly discouraged to use [split ":standard \ compat"]. Instead use
      [difference (standard) (each ["compat"])] so the meaning is clear and so that DSL interpreters are exposed to what you
      actually meant to happen.
  *)

  val set_of : string list -> [ `OrderedSet ] repr
  (** [set_of [arg1; arg2; ...]] are zero or more arguments [arg1], [arg2] and so on. *)

  val standard : [ `OrderedSet ] repr
  (** [standard] are the standard arguments.

      The standard depends on the context in which the arguments are applied. See the relevant documentation
      for the definition of [standard]:

      - {!modules}
    *)

  val split : string -> [ `OrderedSet ] repr
  (** [split "arg1 arg2 ..."] are zero or more arguments [arg1], [arg2] and so on after splitting the given
      string ["arg1 arg2 ..."] according to the
      {{:https://github.com/janestreet/sexplib#lexical-conventions-of-s-expression}Lexical conventions of s-expression}
      for identifying atoms in a list. *)

  val union : [ `OrderedSet ] repr list -> [ `OrderedSet ] repr
  (** [union [set1; set2; ...]] is all the arguments from [set1] and also [set2] and so on. *)

  val difference :
    [ `OrderedSet ] repr -> [ `OrderedSet ] repr -> [ `OrderedSet ] repr
  (** [difference a_set b_set] is all the arguments from [a_set] that are not in [b_set] *)

  (** {3:Libraries Libraries Only} *)

  val virtual_modules : [ `OrderedSet ] repr -> [ `Library ] repr
  (** [virtual_modules ordered_set] specifies what modules for which only an interface would be present, and which
      will have implementations defined in other libraries.

      The virtual modules play the role of interfaces or protocols in
      {{:https://dune.readthedocs.io/en/stable/variants.html#virtual-library}Dune virtual libraries}.

      See {!section-"Ordered-Sets"} for the operations you can perform. *)

  val implements : string -> [ `Library ] repr
  (** [implements libname] specifies the virtual library the current library (the implementing library)
      provides an implementation for.

      See {{:https://dune.readthedocs.io/en/stable/variants.html#virtual-library}Dune virtual libraries}
      for more details. *)

  val default_implementation : string -> [ `Library ] repr
  (** [default_implementation default_impl] selects the default implementation for a virtual library, which
      is enabled after variant resolution if no suitable implementation has been found. *)

  (** {3:Executables Executables Only}
      
      As of this version there are no executable-only clauses. *)

  (** {3:Install Install} *)

  val section : string -> [ `Install ] repr
  (** [section "share"] is the section in which the files will be installed.
      
      The following sections are available:

      - ["lib"] installs by default to ["/lib/<pkgname>/"]
      - ["lib_root"] installs by default to ["/lib/"]
      - ["libexec"] installs by default to ["/lib/<pkgname>/"] with the executable bit set
      - ["libexec_root"] installs by default to ["/lib/"] with the executable bit set
      - ["bin"] installs by default to ["/bin/"] with the executable bit set
      - ["sbin"] installs by default to ["/sbin/"] with the executable bit set
      - ["toplevel"] installs by default to ["/lib/toplevel/"]
      - ["share"] installs by default to ["/share/<pkgname>/"]
      - ["share_root"] installs by default to ["/share/"]
      - ["etc"] installs by default to ["/etc/<pkgname>/"]
      - ["doc"] installs by default to ["/doc/<pkgname>/"]
      - ["stublibs"] installs by default to ["/lib/stublibs/"] with the executable bit set
      - ["man"] installs by default relative to ["/man"] with the destination directory extracted from the extension of the source file (so that installing foo.1 is equivalent to a destination of man1/foo.1)
      - ["misc"] requires files to specify an absolute destination. It will only work when used with opam and the user will be prompted before the installation when it's done via opam. It is deprecated.

      The following sections are not yet available in Dune DSL:
      - ["(site (<package> <site>))"] installs in the <site> directory of <package>. If the prefix isn't the same as the one used when installing <package>, <package> won't find the files.  
  *)

  val install_files : [ `InstallDestination ] repr list -> [ `Install ] repr
  (** {{:https://dune.readthedocs.io/en/stable/dune-files.html#install-1}}. *)

  (** {4 Install Destination} *)

  val destination_file :
    filename:string -> destination:string -> [ `InstallDestination ] repr
  (** {{:https://dune.readthedocs.io/en/stable/dune-files.html#install-1}}.

      [destination_file ~filename ~destination] represents the sub-expression:

      {v
(mylib.el as emacs/site-lisp/mylib.el)
      v}

      in

      {v
(files   (mylib.el as emacs/site-lisp/mylib.el))
      v}
   *)
end
