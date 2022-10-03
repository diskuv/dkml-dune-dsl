module I :
  DkmlDuneDsl.Dune.SYM
    with type 'a repr = Mustache.Json.t -> Sexplib.Sexp.With_layout.t = struct
  open Sexplib.Sexp.With_layout

  type 'a repr = Mustache.Json.t -> Sexplib.Sexp.With_layout.t

  (** {2 Utilities} *)

  let _parameterize ~json s = Mustache.render (Mustache.of_string s) json

  let _atom atom = Atom ({ row = 0; col = 0 }, atom, None)
  (* An [Atom] s-exp without comments or pos *)

  let _list l =
    List
      ( { row = 0; col = 0 },
        List.map (fun sexp -> Sexp sexp) l,
        { row = 0; col = 0 } )
  (* A [List] s-exp without comments or pos inside the list items *)

  let _vararg_of_string ~json token sl =
    _list
      ([ _atom token ]
      @ Stdlib.List.map (fun s -> _atom (_parameterize ~json s)) sl)

  let _arg_of_string ~json token s =
    _list [ _atom token; _atom (_parameterize ~json s) ]

  let _spread json = List.map (fun child -> child json)

  (** {2 Stanzas} *)

  let rule l json = _list ([ _atom "rule" ] @ _spread json l)

  let executable l json = _list ([ _atom "executable" ] @ _spread json l)

  let install l json = _list ([ _atom "install" ] @ _spread json l)

  (** {3 Rules} *)

  let alias s json = _arg_of_string ~json "alias" s

  let targets l json = _vararg_of_string ~json "targets" l

  let target s json = _arg_of_string ~json "target" s

  let deps l json = _list ([ _atom "deps" ] @ _spread json l)

  let action a json = _list [ _atom "action"; a json ]

  (** {4 Dependencies} *)

  let glob_files globstring json = _arg_of_string ~json "glob_files" globstring

  let named_dep ~name dep json =
    _list
      [
        _atom (":" ^ _parameterize ~json name); _atom (_parameterize ~json dep);
      ]

  (** {4 Actions} *)

  let echo msglst json = _vararg_of_string ~json "echo" msglst

  let with_stdout_to file action json =
    _list
      [ _atom "with-stdout-to"; _atom (_parameterize ~json file); action json ]

  let progn l json = _list ([ _atom "progn" ] @ _spread json l)

  let run l json = _vararg_of_string ~json "run" l

  let diff ~actual ~expected json =
    _list
      [
        _atom "diff";
        _atom (_parameterize ~json actual);
        _atom (_parameterize ~json expected);
      ]

  let diff_q ~actual ~expected json =
    _list
      [
        _atom "diff?";
        _atom (_parameterize ~json actual);
        _atom (_parameterize ~json expected);
      ]

  (** {3 Executables} *)

  let public_name s json = _arg_of_string ~json "public_name" s

  let name s json = _arg_of_string ~json "name" s

  let libraries l json = _vararg_of_string ~json "libraries" l

  let modules l json = _vararg_of_string ~json "modules" l

  let modes_byte_exe _json =
    _list [ _atom "modes"; _list [ _atom "byte"; _atom "exe" ] ]

  let ocamlopt_flags l json = _list ([ _atom "ocamlopt_flags" ] @ _spread json l)

  (** {3 Install} *)

  let section s json = _arg_of_string ~json "section" s

  let install_files l json = _list ([ _atom "files" ] @ _spread json l)

  let destination_file ~filename ~destination json =
    _list
      [
        _atom (_parameterize ~json filename);
        _atom "as";
        _atom (_parameterize ~json destination);
      ]
end

(* Pretty printers *)

let plain_hum_config =
  Sexp_pretty.Config.create ~color:false ~interpret_atom_as_sexp:true ()

let pretty = Sexp_pretty.Config.default

(* Mustache *)

type params_avail = No_parameters | Has_parameters

let json_from_argv () : params_avail * Mustache.Json.t =
  match Sys.argv with
  | [||] -> failwith "Sys.argv was empty!"
  | [| _ |] ->
      (* Equivalent to a single-item array (1 item) of an empty object (no parameters):
         {v
            [ {} ]
         v} *)
      (No_parameters, `A [ `O [] ])
  | [| _; filename |] ->
      let ic = open_in filename in
      let x = Ezjsonm.from_channel ic in
      close_in ic;
      (Has_parameters, x)
  | _ -> failwith "usage: show.exe [MUSTACHE_JSON_PARAMETERS]"

(* CLI entry points *)

let do_cli sexp_pretty_config stanza_sexpf_lst =
  (* Get the JSON *)
  let params_avail, json = json_from_argv () in
  (* Parse JSON *)
  let list_of_json_runs =
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
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  let g_run run_idx json_run =
    (* Validate the JSON run (which is passed directly to Mustache) is an object *)
    match json_run with
    | `O _ as validated_json_run ->
        let open Sexplib.Sexp.With_layout in
        let pending_prints = Queue.create () in
        (* Print comment with the Mustache JSON parameters *)
        (match params_avail with
        | No_parameters -> ()
        | Has_parameters ->
            let run_description = Ezjsonm.value_to_string json_run in
            let run_description_l =
              Astring.String.cuts ~sep:"\n" run_description
              |> List.map (fun s -> ";   " ^ s)
            in
            let run_description_commented =
              Printf.sprintf "; Parameters %d\n%s" (run_idx + 1)
                (String.concat "\n" run_description_l)
            in
            let run_comment =
              Comment
                (Plain_comment ({ row = 0; col = 0 }, run_description_commented))
            in
            Queue.add run_comment pending_prints);
        (* Print Dune stanzas *)
        let f_stanza sexpf =
          let sexp = Sexp (sexpf validated_json_run) in
          Queue.add sexp pending_prints
        in
        List.iter f_stanza stanza_sexpf_lst;
        (* Dump everything to formatter *)
        if run_idx > 0 then Format.pp_print_newline fmt ();
        let next () = Queue.take_opt pending_prints in
        Sexp_pretty.Sexp_with_layout.pp_formatter' ~next sexp_pretty_config fmt
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
  List.iteri g_run list_of_json_runs;
  Format.pp_print_flush fmt ();
  Buffer.to_bytes buf |> Bytes.to_string

let plain_hum
    (stanza_sexpf_lst : (Mustache.Json.t -> Sexplib.Sexp.With_layout.t) list) =
  do_cli plain_hum_config stanza_sexpf_lst

let pretty
    (stanza_sexpf_lst : (Mustache.Json.t -> Sexplib.Sexp.With_layout.t) list) =
  do_cli pretty stanza_sexpf_lst
