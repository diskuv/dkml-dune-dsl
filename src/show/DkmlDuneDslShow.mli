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

type out = Sexplib.Sexp.With_layout.t option

module I : DkmlDuneDsl.Dune.SYM with type 'a repr = args -> out
(** An interpreter of the Dune DSL whose interpreted result is a s-exp
    of a valid Dune file parameterizable by Mustache JSON parameters.
    
    Use {!plain_hum} or {!pretty} to show the Dune file. *)

val plain_hum : (args -> out) list -> string
(** Show the DSL expression as a plain, human-readable, valid Dune file *)

val pretty : (args -> out) list -> string
(** Show the DSL expression as a Dune file with colors if your terminal supports it; do not use for writing Dune files! *)
