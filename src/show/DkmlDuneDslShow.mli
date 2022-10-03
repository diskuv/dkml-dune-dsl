module I :
  DkmlDuneDsl.Dune.SYM
    with type 'a repr = Mustache.Json.t -> Sexplib.Sexp.With_layout.t
(** An interpreter of the Dune DSL whose interpreted result is a s-exp
    of a valid Dune file parameterizable by Mustache JSON configuration.
    
    Use {!plain_hum} or {!pretty} to show the Dune file. *)

val plain_hum : (Mustache.Json.t -> Sexplib.Sexp.With_layout.t) list -> string
(** Show the DSL expression as a plain, human-readable, valid Dune file *)

val pretty : (Mustache.Json.t -> Sexplib.Sexp.With_layout.t) list -> string
(** Show the DSL expression as a Dune file with colors if your terminal supports it; do not use for writing Dune files! *)
