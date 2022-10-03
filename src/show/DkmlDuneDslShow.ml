module I :
  DkmlDuneDsl.Dune.SYM with type 'a repr = Mustache.Json.t -> Base.Sexp.t =
struct
  open Base
  open Sexp

  type 'a repr = Mustache.Json.t -> Sexp.t

  (** {2 Utilities} *)

  let _parameterize ~json s = Mustache.render (Mustache.of_string s) json

  let _vararg_of_string ~json token sl =
    List
      ([ Atom token ]
      @ Stdlib.List.map (fun s -> Atom (_parameterize ~json s)) sl)

  let _arg_of_string ~json token s =
    List [ Atom token; Atom (_parameterize ~json s) ]

  let _spread json = List.map ~f:(fun child -> child json)

  (** {2 Stanzas} *)

  let rule l json = List ([ Atom "rule" ] @ _spread json l)

  let executable l json = List ([ Atom "executable" ] @ _spread json l)

  let install l json = List ([ Atom "install" ] @ _spread json l)

  (** {3 Rules} *)

  let alias s json = _arg_of_string ~json "alias" s

  let targets l json = _vararg_of_string ~json "targets" l

  let target s json = _arg_of_string ~json "target" s

  let deps l json = List ([ Atom "deps" ] @ _spread json l)

  let action a json = List [ Atom "action"; a json ]

  (** {4 Dependencies} *)

  let glob_files globstring json = _arg_of_string ~json "glob_files" globstring

  let named_dep ~name dep json =
    List
      [ Atom (":" ^ _parameterize ~json name); Atom (_parameterize ~json dep) ]

  (** {4 Actions} *)

  let echo msglst json = _vararg_of_string ~json "echo" msglst

  let with_stdout_to file action json =
    List [ Atom "with-stdout-to"; Atom (_parameterize ~json file); action json ]

  let progn l json = List ([ Atom "progn" ] @ _spread json l)

  let run l json = _vararg_of_string ~json "run" l

  let diff ~actual ~expected json =
    List
      [
        Atom "diff";
        Atom (_parameterize ~json actual);
        Atom (_parameterize ~json expected);
      ]

  let diff_q ~actual ~expected json =
    List
      [
        Atom "diff?";
        Atom (_parameterize ~json actual);
        Atom (_parameterize ~json expected);
      ]

  (** {3 Executables} *)

  let public_name s json = _arg_of_string ~json "public_name" s

  let name s json = _arg_of_string ~json "name" s

  let libraries l json = _vararg_of_string ~json "libraries" l

  let modules l json = _vararg_of_string ~json "modules" l

  let modes_byte_exe _json =
    List [ Atom "modes"; List [ Atom "byte"; Atom "exe" ] ]

  let ocamlopt_flags l json = List ([ Atom "ocamlopt_flags" ] @ _spread json l)

  (** {3 Install} *)

  let section s json = _arg_of_string ~json "section" s

  let install_files l json = List ([ Atom "files" ] @ _spread json l)

  let destination_file ~filename ~destination json =
    List
      [
        Atom (_parameterize ~json filename);
        Atom "as";
        Atom (_parameterize ~json destination);
      ]
end

(* Pretty printers *)

let plain_hum_config =
  Sexp_pretty.Config.create ~color:false ~interpret_atom_as_sexp:true ()

let pretty = Sexp_pretty.Config.default

(* Mustache *)

let json_from_argv () : Mustache.Json.t =
  match Sys.argv with
  | [||] -> failwith "Sys.argv was empty!"
  | [| _ |] ->
      (* Equivalent to a single-item array (1 item) of an empty object (no parameters):
         {v
            [ {} ]
         v} *)
      `A [ `O [] ]
  | [| _; filename |] ->
      let ic = open_in filename in
      let x = Ezjsonm.from_channel ic in
      close_in ic;
      x
  | _ -> failwith "usage: show.exe [MUSTACHE_JSON_PARAMETERS]"

(* CLI entry points *)

let do_cli sexp_pretty_config stanza_sexpf_lst =
  (* Parse JSON *)
  let list_of_json_runs =
    (* Get the JSON *)
    let json = json_from_argv () in
    (* Validate it is an array (one for each parameterized run of the interpreter) *)
    match json with
    | `A runs -> runs
    | _ ->
        let msg =
          Printf.sprintf
            "The JSON parameter file is not an array. The most basic JSON \
             parameter file is: [ {} ]. Instead the parameter file was: %s"
            (Ezjsonm.to_string json)
        in
        failwith msg
  in
  let g json_run =
    (* Validate the JSON run (which is passed directly to Mustache) is an object *)
    match json_run with
    | `O _ as validated_json_run ->
        let f sexpf =
          let sexp = sexpf validated_json_run in
          Sexp_pretty.(pretty_string sexp_pretty_config sexp)
        in
        List.map f stanza_sexpf_lst
    | _ ->
        let msg =
          Printf.sprintf
            "The JSON parameter file is not an array of objects. The most \
             basic JSON parameter file is: [ {} ]. Instead the parameter file \
             was: %s"
            (Ezjsonm.value_to_string json_run)
        in
        failwith msg
  in
  String.concat "\n" @@ List.flatten @@ List.map g list_of_json_runs

let plain_hum (stanza_sexpf_lst : (Mustache.Json.t -> Sexplib0.Sexp.t) list) =
  do_cli plain_hum_config stanza_sexpf_lst

let pretty (stanza_sexpf_lst : (Mustache.Json.t -> Sexplib0.Sexp.t) list) =
  do_cli pretty stanza_sexpf_lst
