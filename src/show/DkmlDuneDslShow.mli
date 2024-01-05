type args = {
  entire_params_file : Mustache.Json.t;
      (** The entire parameters file as a single JSON object. At minimum the JSON object
          should contain the ["param-sets"] array with at least one (possibly empty) object:
{v
{
  "param-sets": [{}]
}
v} *)
  params : Mustache.Json.t;
      (** An array item in the ["param-sets"] array that is the source of values for a Mustache
          template. Each array item is a JSON object.
          In the {{:http://mustache.github.io/mustache.5.html}canonical Mustache documentation}
          the JSON object is called a "hash". *)
  params_idx : int;
      (** Zero-based index of [params] in the user-supplied ["param-sets"]. *)
}

type out = Sexplib.Sexp.With_layout.t list

module I : DkmlDuneDsl.Dune.SYM with type 'a repr = args -> out
(** An interpreter of the Dune DSL whose interpreted result is a s-exp
    of a valid Dune file parameterizable by Mustache JSON parameters.
    
    Use {!plain_hum} or {!pretty} to show the Dune file. *)

val plain_hum : (args -> out) list -> string
(** Show the DSL expression as a plain, human-readable, valid Dune file.

    The command line arguments [Sys.argv] are parsed and the first
    argument, if any, is used as the JSON parameters file. *)

val pretty : (args -> out) list -> string
(** Show the DSL expression as a Dune file with colors if your terminal supports
    it; do not use for writing Dune files! *)

val no_parameters_json : Mustache.Json.t
(** The contents of a JSON parameters file that has no parameters.
    
    Equivalent to a single-item array (1 item) of an empty object. *)

val plain_hum_with_params : Mustache.Json.t -> (args -> out) list -> string
(** [plain_hum_with_params params res] show the DSL expression [res] as a
    plain, human-readable, valid Dune file using the JSON parameters [params].

    [params] should have the same structure as a JSON parameters file.

    No command line arguments [Sys.argv] are parsed. *)
