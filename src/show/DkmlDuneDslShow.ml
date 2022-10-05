type args = {
  params : Mustache.Json.t;
  params_idx : int;
  entire_params_file : Mustache.Json.t;
}

type out = Sexplib.Sexp.With_layout.t option

module I : DkmlDuneDsl.Dune.SYM with type 'a repr = args -> out = struct
  open Sexplib.Sexp.With_layout

  type 'a repr = args -> out

  type compilation_mode = Byte | Native | Best

  type binary_kind = C | Exe | Object | Shared_object | Js | Plugin

  (** {2 Utilities} *)

  let _quote_if_needed s =
    let s' = String.escaped s in
    if String.equal s s' then s else "\"" ^ s' ^ ""

  (** [_parameterize ~args s] renders any Mustache expressions in [s] using [json], and quotes
      the result if necessary *)
  let _parameterize ~args s =
    let template =
      try Mustache.of_string s
      with e ->
        let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
        let underscores = String.map (fun _c -> '_') s in
        Printf.eprintf
          "FATAL: Could not decode the string value:\n\n\
           %s\n\
           %s\n\
           %s\n\n\
           as a valid Mustache template: %s%s\n"
          underscores s underscores msg stack;
        raise e
    in
    let rendered = Mustache.render template args.params in
    _quote_if_needed @@ rendered

  let zero_pos = { row = 0; col = 0 }

  let _atom atom = Some (Atom (zero_pos, atom, None))
  (* An [Atom] s-exp without comments or pos *)

  let _list l =
    Some
      (List
         ( zero_pos,
           List.filter_map
             (function None -> None | Some sexp -> Some (Sexp sexp))
             l,
           zero_pos ))
  (* A [List] s-exp without comments or pos inside the list items *)

  let _vararg_of_string ~args token sl =
    _list
      ([ _atom token ]
      @ Stdlib.List.map (fun s -> _atom (_parameterize ~args s)) sl)

  let _arg_of_string ~args token s =
    _list [ _atom token; _atom (_parameterize ~args s) ]

  let _spread args = List.map (fun child -> child args)

  let _ordset_atom_list l =
    List
      (zero_pos, List.map (fun i -> Sexp (Atom (zero_pos, i, None))) l, zero_pos)

  let _arg_of_ordset token (ordset : out) : t_or_comment list =
    match ordset with
    | Some (Atom (_, atom, _)) -> [ Sexp (_ordset_atom_list [ token; atom ]) ]
    | Some (List (_, l, _)) -> [ Sexp (Atom (zero_pos, token, None)) ] @ l
    | None -> [ Sexp (_ordset_atom_list [ token ]) ]

  let _atomize_sexp = Sexplib.Sexp.to_string

  let _string_of_atoms_to_sexp_list s = Sexplib.Sexp.of_string ("(" ^ s ^ ")")

  (** {2 Stanzas} *)

  let rule l args = _list ([ _atom "rule" ] @ _spread args l)

  let executable l args = _list ([ _atom "executable" ] @ _spread args l)

  let library l args = _list ([ _atom "library" ] @ _spread args l)

  let install l args = _list ([ _atom "install" ] @ _spread args l)

  let pragma statement stanza args =
    match (statement, args.params_idx) with
    | "once", 0 ->
        (* never do parameter replacement under "once"; that would make once dependent
           on the order of the parameter set, which is too dangerous for a regular user.
           instead we give the very useful entire parameters file *)
        stanza { args with params = args.entire_params_file }
    | "once", _idx ->
        (* exclude the stanza if we are repeating more than once *)
        None
    | _ -> stanza args

  (** {3 Rules} *)

  let alias s args = _arg_of_string ~args "alias" s

  let targets l args = _vararg_of_string ~args "targets" l

  let target s args = _arg_of_string ~args "target" s

  let deps l args = _list ([ _atom "deps" ] @ _spread args l)

  let action a args = _list [ _atom "action"; a args ]

  (** {4 Dependencies} *)

  let glob_files globstring args = _arg_of_string ~args "glob_files" globstring

  let named_dep ~name dep args =
    _list
      [
        _atom (":" ^ _parameterize ~args name); _atom (_parameterize ~args dep);
      ]

  (** {4 Actions} *)

  let echo msglst args = _vararg_of_string ~args "echo" msglst

  let with_stdout_to file action args =
    _list
      [ _atom "with-stdout-to"; _atom (_parameterize ~args file); action args ]

  let progn l args = _list ([ _atom "progn" ] @ _spread args l)

  let run l args = _vararg_of_string ~args "run" l

  let diff ~actual ~expected args =
    _list
      [
        _atom "diff";
        _atom (_parameterize ~args actual);
        _atom (_parameterize ~args expected);
      ]

  let diff_q ~actual ~expected args =
    _list
      [
        _atom "diff?";
        _atom (_parameterize ~args actual);
        _atom (_parameterize ~args expected);
      ]

  let setenv ~name ~value action args =
    _list
      [
        _atom "setenv";
        _atom (_parameterize ~args name);
        _atom (_parameterize ~args value);
        action args;
      ]

  (** {3 Executables and Libraries} *)

  let public_name s args = _arg_of_string ~args "public_name" s

  let name s args = _arg_of_string ~args "name" s

  let libraries l args =
    let l' =
      Stdlib.List.map
        (function
          | `L s -> [ _atom (_parameterize ~args s) ]
          | `SplitL s -> (
              match _string_of_atoms_to_sexp_list s with
              | Atom a -> [ _atom (_parameterize ~args a) ]
              | List [] -> []
              | List l ->
                  List.map
                    (fun sexp ->
                      _atom (_atomize_sexp sexp |> _parameterize ~args))
                    l))
        l
    in
    _list ([ _atom "libraries" ] @ List.flatten l')

  let show_compilation_mode = function
    | Byte -> "byte"
    | Native -> "native"
    | Best -> "best"

  let show_binary_kind = function
    | C -> "c"
    | Exe -> "exe"
    | Object -> "object"
    | Shared_object -> "shared_object"
    | Js -> "js"
    | Plugin -> "plugin"

  let _mode = function
    | `C -> _atom "c"
    | `Exe -> _atom "exe"
    | `Object -> _atom "object"
    | `Shared_object -> _atom "shared_object"
    | `Byte -> _atom "byte"
    | `Native -> _atom "native"
    | `Js -> _atom "js"
    | `Plugin -> _atom "plugin"
    | `Byte_complete -> _atom "byte_complete"
    | `Mode (compilation_mode, binary_kind) ->
        _list
          [
            _atom (show_compilation_mode compilation_mode);
            _atom (show_binary_kind binary_kind);
          ]

  let modes l _args = _list ([ _atom "modes" ] @ List.map _mode l)

  let modules (ordset : [ `OrderedSet ] repr) _args =
    match ordset _args with
    | None -> _list [ _atom "modules" ]
    | Some v ->
        let v' = _arg_of_ordset "modules" (Some v) in
        Some (List (zero_pos, v', zero_pos))

  let ocamlopt_flags l args = _list ([ _atom "ocamlopt_flags" ] @ _spread args l)

  let preprocess spec args = _list [ _atom "preprocess"; spec args ]

  (** {4 Preprocessing} *)

  let no_preprocessing _args = _atom "no_preprocessing"

  let pps l args = _vararg_of_string ~args "pps" l

  let staged_pps l args = _vararg_of_string ~args "staged_pps" l

  let future_syntax _args = _atom "future_syntax"

  (** {4 Ordered Sets} *)

  (* An ordered set is a non-empty sexp list of atoms and other ordered sets, or it is empty *)

  let set_of l args : out =
    match l with
    | [] -> None
    | l ->
        let l' = List.map (fun s -> _parameterize ~args s) l in
        Some (_ordset_atom_list l')

  let standard _args : out = Some (Atom (zero_pos, ":standard", None))

  let split s args : out =
    (* Split AFTER evaluating the parameters *)
    let posteval = _parameterize ~args s in
    (* Okay. Now is good to split *)
    match _string_of_atoms_to_sexp_list posteval with
    | Atom s -> Some (Atom (zero_pos, s, None))
    | List [] -> None
    | List l ->
        let l' = List.map _atomize_sexp l in
        set_of l' args

  let difference a_set b_set args : out =
    match (a_set args, b_set args) with
    | None, _ -> (* A - B = {} when A = {} *) None
    | Some a, None -> (* A - B = A when B = {} *) Some a
    | Some a, Some b ->
        Some
          (List
             ( zero_pos,
               [ Sexp a; Sexp (Atom (zero_pos, "\\", None)); Sexp b ],
               zero_pos ))

  let union (sets : [ `OrderedSet ] repr list) args : out =
    match List.filter_map (fun child -> child args) sets with
    | [] -> None
    | l -> Some (List (zero_pos, List.map (fun sexp -> Sexp sexp) l, zero_pos))

  (** {3 Install} *)

  let section s args = _arg_of_string ~args "section" s

  let install_files l args = _list ([ _atom "files" ] @ _spread args l)

  let destination_file ~filename ~destination args =
    _list
      [
        _atom (_parameterize ~args filename);
        _atom "as";
        _atom (_parameterize ~args destination);
      ]
end

(* Pretty printers *)

let plain_hum_config = Sexp_pretty.Config.create ~color:false ()

let pretty = Sexp_pretty.Config.default

(* Mustache *)

type params_avail = No_parameters | Has_parameters

(** Equivalent to a single-item array (1 item) of an empty object (no parameters) *)
let minimal_params_file = "{ \"param-sets\": [ {} ] }"

let json_from_argv () : params_avail * Mustache.Json.t =
  match Sys.argv with
  | [||] -> failwith "Sys.argv was empty!"
  | [| _ |] -> (No_parameters, Ezjsonm.from_string minimal_params_file)
  | [| _; filename |] ->
      let ic = open_in filename in
      let json = Ezjsonm.from_channel ic in
      close_in ic;
      (Has_parameters, json)
  | _ -> failwith "usage: show.exe [MUSTACHE_JSON_PARAMETERS]"

(* CLI entry points *)

let do_cli sexp_pretty_config (stanza_sexpf_lst : (args -> out) list) =
  (* Get the JSON *)
  let params_avail, entire_params_file = json_from_argv () in
  (* Parse JSON *)
  let param_sets =
    (* Validate it is an object *)
    (match entire_params_file with
    | `O _ -> ()
    | _ ->
        let msg =
          Printf.sprintf
            "The JSON parameter file is not a JSON object. A minimal JSON \
             parameter file is: %s. Instead the parameter file was: %s"
            minimal_params_file
            (Ezjsonm.to_string entire_params_file)
        in
        prerr_endline @@ "FATAL: " ^ msg;
        failwith msg);
    (* Validate it has a param-sets array, and return it *)
    match
      Ezjsonm.find_opt (Ezjsonm.value entire_params_file) [ "param-sets" ]
    with
    | Some (`A param_sets) -> param_sets
    | Some value ->
        let msg =
          Printf.sprintf
            "The JSON parameter file's \"param-sets\" field is not an array. A \
             minimal JSON parameter file is: %s. Instead the field was: %s"
            minimal_params_file
            (Ezjsonm.value_to_string value)
        in
        prerr_endline @@ "FATAL: " ^ msg;
        failwith msg
    | None ->
        let msg =
          Printf.sprintf
            "The JSON parameter file's \"param-sets\" field was not present. A \
             minimal JSON parameter file is: %s. Instead the parameter file \
             was: %s"
            minimal_params_file
            (Ezjsonm.to_string entire_params_file)
        in
        prerr_endline @@ "FATAL: " ^ msg;
        failwith msg
  in
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  let g_param_set params_idx param_set =
    (* Validate the JSON run (which is passed directly to Mustache) is an object *)
    match param_set with
    | `O _ as validated_param_set ->
        let open Sexplib.Sexp.With_layout in
        let pending_prints = Queue.create () in
        (* Print comment with the Mustache JSON parameters *)
        (match params_avail with
        | No_parameters -> ()
        | Has_parameters ->
            (* Describe the parameter set *)
            let ps_description = Ezjsonm.value_to_string param_set in
            let ps_description_l =
              Astring.String.cuts ~sep:"\n" ps_description
              |> List.map (fun s -> ";   " ^ s)
            in
            let ps_description_commented =
              Printf.sprintf "; Parameter Set =\n%s"
                (String.concat "\n" ps_description_l)
            in
            let ps_comment =
              Comment
                (Plain_comment ({ row = 0; col = 0 }, ps_description_commented))
            in
            Queue.add ps_comment pending_prints);
        (* Print Dune stanzas *)
        let f_stanza sexpf =
          match
            sexpf
              { entire_params_file; params = validated_param_set; params_idx }
          with
          | None -> ()
          | Some sexp -> Queue.add (Sexp sexp) pending_prints
        in
        List.iter f_stanza stanza_sexpf_lst;
        (* Dump everything to formatter *)
        if params_idx > 0 then Format.pp_print_newline fmt ();
        let next () = Queue.take_opt pending_prints in
        Sexp_pretty.Sexp_with_layout.pp_formatter' ~next sexp_pretty_config fmt
    | _ ->
        let msg =
          Printf.sprintf
            "The JSON parameter file is not an array of objects. The most \
             basic JSON parameter file is: [ {} ]. Instead the parameter file \
             was: %s"
            (Ezjsonm.value_to_string param_set)
        in
        failwith msg
  in
  List.iteri g_param_set param_sets;
  Format.pp_print_flush fmt ();
  Buffer.to_bytes buf |> Bytes.to_string

let plain_hum (stanza_sexpf_lst : (args -> out) list) =
  do_cli plain_hum_config stanza_sexpf_lst

let pretty (stanza_sexpf_lst : (args -> out) list) =
  do_cli pretty stanza_sexpf_lst
