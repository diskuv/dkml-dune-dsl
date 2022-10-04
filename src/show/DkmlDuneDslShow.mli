type args = {
  params : Mustache.Json.t;
  params_idx : int;
      (** Zero-based index of [params] in the user-supplied parameter set *)
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
