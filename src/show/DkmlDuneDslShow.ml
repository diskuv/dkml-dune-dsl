open Astring

type args = {
  entire_params_file : Mustache.Json.t;
  params : Mustache.Json.t;
  params_idx : int;
}

type out = Sexplib.Sexp.With_layout.t list

module I : DkmlDuneDsl.Dune.SYM with type 'a repr = args -> out = struct
  open Sexplib.Sexp.With_layout

  type 'a repr = args -> out
  type compilation_mode = Byte | Native | Best
  type binary_kind = C | Exe | Object | Shared_object | Js | Plugin

  (** {2 Utilities} *)

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
    Mustache.render template args.params

  let zero_pos = { row = 0; col = 0 }
  let _atom atom = [ Atom (zero_pos, atom, None) ]
  (* An [Atom] s-exp without comments or pos *)

  let _list l =
    [ List (zero_pos, List.map (fun i -> Sexp i) (List.flatten l), zero_pos) ]
  (* A [List] s-exp without comments or pos inside the list items *)

  let _atomize_sexp = Sexplib.Sexp.to_string
  let _string_of_atoms_to_sexp_list s = Sexplib.Sexp.of_string ("(" ^ s ^ ")")

  let _splittable_string_list_and_args_as_repr_list l args =
    List.flatten
    @@ List.map
         (function
           | `S s -> [ _atom (_parameterize ~args s) ]
           | `Split s -> (
               let s' = _parameterize ~args s in
               match _string_of_atoms_to_sexp_list s' with
               | Atom a -> [ _atom a ]
               | List [] -> []
               | List l -> List.map (fun sexp -> _atom (_atomize_sexp sexp)) l))
         l

  (* let _splittable_string_list_as_repr_list l =
     List.flatten
     @@ List.map
          (function
            | `S s -> [ fun args -> _atom (_parameterize ~args s) ]
            | `Split s -> (
                let s' = _parameterize ~args s in
                match _string_of_atoms_to_sexp_list s' with
                | Atom a -> [ fun args -> _atom (_parameterize ~args a) ]
                | List [] -> []
                | List l -> List.map (fun sexp -> _atom (_atomize_sexp sexp)) l))
          l *)

  let _arg_of_string ~args token s =
    _list [ _atom token; _atom (_parameterize ~args s) ]

  let _vararg_of_string ~args token sl =
    _list
      ([ _atom token ]
      @ Stdlib.List.map (fun s -> _atom (_parameterize ~args s)) sl)

  let _vararg_of_splittable_string ?none_when_empty ~args token sl =
    match
      (none_when_empty, _splittable_string_list_and_args_as_repr_list sl args)
    with
    | Some (), l when List.map Fun.id (List.flatten l) |> List.length = 0 -> []
    | _, strings -> _list ([ _atom token ] @ strings)

  let _spread args = List.map (fun child -> child args)

  let _ordset_atom_list ?p1 ?p2 l =
    let p1 = Option.value ~default:zero_pos p1 in
    let p2 = Option.value ~default:zero_pos p2 in
    List (p1, List.map (fun i -> Sexp (Atom (p1, i, None))) l, p2)

  (** An empty set. For now the simplest expression is (:standard \ :standard)  *)
  let _empty_set =
    [
      Sexp (Atom (zero_pos, ":standard", None));
      Sexp (Atom (zero_pos, "\\", None));
      Sexp (Atom (zero_pos, ":standard", None));
    ]

  let _pp_item fmt v =
    let config = Sexp_pretty.Config.create ~color:false () in
    Fmt.pf fmt "%s" (Sexp_pretty.Sexp_with_layout.pretty_string config (Sexp v))

  (** Before we convert an ordset into a ['a repr = t_or_comment list] we
      need to optimize it so it avoids the following:

      Dune on https://dune.readthedocs.io/en/stable/concepts.html#ordered-set-language says:

      > Note that inside an ordered set, the first element of a list cannot be an atom except
      > if it starts with - or :.

      The solution given will not work with our s-exp pretty printer (or any!):

      > If you want to write a list where the first element doesn’t start with -, you can simply
      > quote it: ("x" y z).

      So:
      
      + all one-element lists are promoted into atoms (which is not sufficient by itself)
      + all lists with an atom as the first argument are prepended with the empty set
        expression (:standard \ :standard)
  *)

  (** Post order traversal so leaves are visited first. That way [((((a))))] can
        be promoted into [a]. *)
  let rec promote_one_element_lists ~name ~inject_empty_set (orig_ordset : out)
      (current_ordset : out) : t option =
    match current_ordset with
    | [ (Atom _ as a) ] -> Some a
    | [] -> None
    | [ List (p1, l, p2) ] -> (
        (* visit children first *)
        let l' =
          List.map
            (function
              | Comment c -> Comment c
              | Sexp sexp -> (
                  match
                    promote_one_element_lists ~name ~inject_empty_set
                      orig_ordset [ sexp ]
                  with
                  | None ->
                      failwith
                        "Illegal state. promote_one_element_lists was None"
                  | Some sexp' -> Sexp sexp'))
            l
        in
        (* post-order, simplify any one argument lists *)
        match l' with
        | [] -> None
        | [ Sexp one_arg ] -> Some one_arg
        (* nit: [ Comment _; Sexp _ ], [ Comment _; Comment _; Sexp _ ], etc. are not collapsed
           with this (match l') code. Need to visit all the elements of l', but we can ignore
           since we don't put comments in ordered sets. *)
        | Sexp (Atom (_, _, _)) :: _tl as all_args when inject_empty_set ->
            (* Add the empty set to lists with an atomic first argument . It must be
               isolated with enclosing parenthese so the different operator "\" does not
               affect the remaining arguments "*)
            let isolated_empty_set =
              Sexp (List (zero_pos, _empty_set, zero_pos))
            in
            Some (List (p1, isolated_empty_set :: all_args, p1))
        | l'' -> Some (List (p1, l'', p2)))
    | _ ->
        Fmt.failwith "Illegal argument. The %s was: %a" name (Fmt.list _pp_item)
          orig_ordset

  let _arg_of_ordset token (ordset : out) : t list =
    (* promote, and then restructure so the result is a ['a repr] *)
    match
      promote_one_element_lists ~name:"ordset" ~inject_empty_set:true ordset
        ordset
    with
    | Some (Atom (p1, atom, _)) ->
        [ _ordset_atom_list ~p1 ~p2:p1 [ token; atom ] ]
    | Some (List (p1, l, p2)) ->
        [ List (p1, Sexp (Atom (p1, token, None)) :: l, p2) ]
    | None -> [ _ordset_atom_list [ token ] ]

  let _arg_of_boolean_lang token (ordset : out) : t list =
    (* promote, and then restructure so the result is a ['a repr] *)
    match
      promote_one_element_lists ~name:"boolean language" ~inject_empty_set:false
        ordset ordset
    with
    | Some (Atom (p1, atom, _)) ->
        [ _ordset_atom_list ~p1 ~p2:p1 [ token; atom ] ]
    | Some (List (p1, l, p2)) ->
        [
          List
            ( p1,
              Sexp (Atom (p1, token, None)) :: [ Sexp (List (p1, l, p2)) ],
              p2 );
        ]
    | None -> [ _ordset_atom_list [ token ] ]

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
        []
    | _ -> stanza args

  (** {3 Rules} *)

  let alias s args = _arg_of_string ~args "alias" s
  let targets l args = _vararg_of_splittable_string ~args "targets" l
  let target s args = _arg_of_string ~args "target" s
  let deps l args = _list ([ _atom "deps" ] @ _spread args l)
  let action a args = _list [ _atom "action"; a args ]

  let rule_enabled_if (boolean_lang : [ `BooleanLanguage ] repr) args =
    let final_boolean_lang = boolean_lang args in
    _arg_of_boolean_lang "enabled_if" final_boolean_lang

  type mode = Standard | Fallback | Promote

  let mode mode _args =
    let mode_args =
      match mode with
      | Standard -> [ _atom "standard" ]
      | Fallback -> [ _atom "fallback" ]
      | Promote -> [ _atom "promote" ]
    in
    _list ([ _atom "mode" ] @ mode_args)

  (** {4 Boolean Language} *)

  (** {4 Dependencies} *)

  let glob_files globstring args = _arg_of_string ~args "glob_files" globstring

  let named_dep ~name dep args =
    _list
      [
        _atom (":" ^ _parameterize ~args name); _atom (_parameterize ~args dep);
      ]

  let alias_dep alias args = _arg_of_string ~args "alias" alias
  let file_dep file args = _atom (_parameterize ~args file)

  let file_deps l args =
    List.flatten (_splittable_string_list_and_args_as_repr_list l args)

  (** {4 Actions} *)

  let echo msglst args = _vararg_of_string ~args "echo" msglst

  let with_stdin_from file action args =
    _list
      [ _atom "with-stdin-from"; _atom (_parameterize ~args file); action args ]

  let with_stdout_to file action args =
    _list
      [ _atom "with-stdout-to"; _atom (_parameterize ~args file); action args ]

  let progn l args = _list ([ _atom "progn" ] @ _spread args l)
  let copy ~src ~dest args = _vararg_of_string ~args "copy" [ src; dest ]
  let copy_with_source_directive ~src ~dest args = _vararg_of_string ~args "copy#" [ src; dest ]
  let run l args = _vararg_of_splittable_string ~args "run" l

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
  let package s args = _arg_of_string ~args "package" s

  let libraries l args =
    _list
      ([ _atom "libraries" ]
      @ _splittable_string_list_and_args_as_repr_list l args)

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

  let modules (ordset : [ `OrderedSet ] repr) args =
    let final_ordset = ordset args in
    _arg_of_ordset "modules" final_ordset

  let ocamlopt_flags l args = _list ([ _atom "ocamlopt_flags" ] @ _spread args l)

  let wrapped b _args =
    _list [ _atom "wrapped"; _atom (if b then "true" else "false") ]

  let preprocess spec args =
    (* Dune does not accept empty-arg (preprocess) so we remove it *)
    match spec args with [] -> [] | v -> _list [ _atom "preprocess"; v ]

  (** {4 Preprocessing} *)

  let no_preprocessing _args = _atom "no_preprocessing"

  let pps l args =
    (* Dune does not accept empty-arg (preprocess (pps)) so we remove it *)
    _vararg_of_splittable_string ~none_when_empty:() ~args "pps" l

  let staged_pps l args =
    (* Dune does not accept empty-arg (preprocess (staged_pps)) so we remove it *)
    _vararg_of_splittable_string ~none_when_empty:() ~args "staged_pps" l

  let future_syntax _args = _atom "future_syntax"

  (** {4 Ordered Sets} *)

  (* An ordered set is a non-empty sexp list of atoms and other ordered sets, or it is empty *)

  let set_of l args : out =
    match l with
    | [] -> []
    | l ->
        let l' = List.map (fun s -> _parameterize ~args s) l in
        [ _ordset_atom_list l' ]

  let standard _args : out = [ Atom (zero_pos, ":standard", None) ]

  let split s args : out =
    (* Split AFTER evaluating the parameters *)
    let posteval = _parameterize ~args s in
    (* Okay. Now is good to split *)
    match _string_of_atoms_to_sexp_list posteval with
    | Atom s -> [ Atom (zero_pos, s, None) ]
    | List [] -> []
    | List l ->
        let l' = List.map _atomize_sexp l in
        set_of l' args

  let difference a_set b_set args : out =
    match (a_set args, b_set args) with
    | [], _ -> (* A - B = {} when A = {} *) []
    | [ a ], [] -> (* A - B = A when B = {} *) [ a ]
    | [ a ], [ b ] ->
        [
          List
            ( zero_pos,
              [ Sexp a; Sexp (Atom (zero_pos, "\\", None)); Sexp b ],
              zero_pos );
        ]
    | [ _ ], _ ->
        failwith "Cannot take (difference a b) when 'a' has more than one value"
    | _, [ _ ] ->
        failwith "Cannot take (difference a b) when 'b' has more than one value"
    | _, _ ->
        failwith
          "Cannot take (difference a b) when both 'a' and 'b' have more than \
           one value. Both 'a' and 'b' must have zero or one value"

  let union (sets : [ `OrderedSet ] repr list) args : out =
    match List.map (fun child -> child args) sets with
    | [] -> []
    | l ->
        [
          List
            ( zero_pos,
              List.map (fun sexp -> Sexp sexp) (List.flatten l),
              zero_pos );
        ]

  let template s args = [ Atom (zero_pos, _parameterize ~args s, None) ]

  let all (booleans : [ `BooleanLanguage ] repr list) args : out =
    match List.map (fun child -> child args) booleans with
    | [] -> failwith "(all) must contain at least one boolean expression"
    | l ->
        let l' = List.flatten l in
        let l' = Atom (zero_pos, "and", None) :: l' in
        [ List (zero_pos, List.map (fun sexp -> Sexp sexp) l', zero_pos) ]

  let any (booleans : [ `BooleanLanguage ] repr list) args : out =
    match List.map (fun child -> child args) booleans with
    | [] -> failwith "(or) must contain at least one boolean expression"
    | l ->
        let l' = List.flatten l in
        let l' = Atom (zero_pos, "or", None) :: l' in
        [ List (zero_pos, List.map (fun sexp -> Sexp sexp) l', zero_pos) ]

  let not (a : [ `BooleanLanguage ] repr) args : out =
    let inner = [ Atom (zero_pos, "not", None) ] @ a args in
    [ List (zero_pos, List.map (fun sexp -> Sexp sexp) inner, zero_pos) ]

  let binary_op op (a : [ `BooleanLanguage ] repr)
      (b : [ `BooleanLanguage ] repr) args : out =
    let inner = [ Atom (zero_pos, op, None) ] @ a args @ b args in
    [ List (zero_pos, List.map (fun sexp -> Sexp sexp) inner, zero_pos) ]

  let eq = binary_op "="
  let ne = binary_op "<>"
  let ge = binary_op ">="
  let le = binary_op "<="
  let gt = binary_op ">"
  let lt = binary_op "<"

  (** {3:Libraries Libraries} *)

  let virtual_modules (ordset : [ `OrderedSet ] repr) args =
    let final_ordset = ordset args in
    _arg_of_ordset "virtual_modules" final_ordset

  let implements libname args = _arg_of_string ~args "implements" libname

  let default_implementation impl args =
    _arg_of_string ~args "default_implementation" impl

  (** {3:Executables Executables} *)

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

type params_content = No_parameters | Has_parameters of Mustache.Json.t

let json_from_argv () : params_content =
  match Sys.argv with
  | [||] -> failwith "Sys.argv was empty!"
  | [| _ |] -> No_parameters
  | [| _; filename |] ->
      let ic = open_in filename in
      let json = Ezjsonm.from_channel ic in
      close_in ic;
      Has_parameters json
  | _ -> failwith "usage: show.exe [MUSTACHE_JSON_PARAMETERS]"

(* CLI entry points *)

let known_toplevel_keys = [ "param-sets" ]
let no_parameters_json = `O [ ("param-sets", `A [ `O [] ]) ]
let no_parameters_text = Ezjsonm.to_string no_parameters_json

(** Validate parameters file is an object, with keys having underscores or
    being part of the known toplevel keys *)
let validate_params_file_is_json_object = function
  | `O l as entire_params_file ->
      List.iter
        (fun (key, _value) ->
          if String.is_prefix ~affix:"_" key then ()
          else if List.mem key known_toplevel_keys then ()
          else
            let msg =
              Format.asprintf
                "@[The JSON parameter file has a toplevel key @['%s'@]@ that \
                 is not part of the known keys@ (@[%a@])@ nor does it start \
                 with an underscore.@ Start your key with an underscore if you \
                 want your own custom key.@ @[Example: @['_%s'@]@].@ The \
                 parameter file was:@ @[%s@]@]"
                key
                (Format.pp_print_list
                   ~pp_sep:(fun fmt _v -> Format.pp_print_string fmt ", ")
                   Format.pp_print_string)
                known_toplevel_keys key
                (Ezjsonm.to_string entire_params_file)
            in
            prerr_endline @@ "FATAL: " ^ msg;
            failwith msg)
        l
  | entire_params_file ->
      let msg =
        Printf.sprintf
          "The JSON parameter file is not a JSON object. A minimal JSON \
           parameter file is: %s. Instead the parameter file was: %s"
          no_parameters_text
          (Ezjsonm.to_string entire_params_file)
      in
      prerr_endline @@ "FATAL: " ^ msg;
      failwith msg

(** Validate parameters file has a ["param-sets"] array, and return it *)
let parse_param_sets params_content =
  match params_content with
  | No_parameters -> []
  | Has_parameters entire_params_file -> (
      validate_params_file_is_json_object entire_params_file;
      match
        Ezjsonm.find_opt (Ezjsonm.value entire_params_file) [ "param-sets" ]
      with
      | Some (`A param_sets) -> param_sets
      | Some value ->
          let msg =
            Printf.sprintf
              "The JSON parameter file's \"param-sets\" field is not an array. \
               A minimal JSON parameter file is: %s. Instead the field was: %s"
              no_parameters_text
              (Ezjsonm.value_to_string value)
          in
          prerr_endline @@ "FATAL: " ^ msg;
          failwith msg
      | None ->
          let msg =
            Printf.sprintf
              "The JSON parameter file's \"param-sets\" field was not present. \
               A minimal JSON parameter file is: %s. Instead the parameter \
               file was: %s"
              no_parameters_text
              (Ezjsonm.to_string entire_params_file)
          in
          prerr_endline @@ "FATAL: " ^ msg;
          failwith msg)

let render params_content sexp_pretty_config
    (stanza_sexpf_lst : (args -> out) list) =
  (* Parse JSON *)
  let param_sets = parse_param_sets params_content in
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  let g_param_set params_idx param_set =
    (* Validate the JSON run (which is passed directly to Mustache) is an object *)
    match param_set with
    | `O _ as validated_param_set ->
        let open Sexplib.Sexp.With_layout in
        let pending_prints = Queue.create () in
        (* Print comment with the Mustache JSON parameters *)
        let entire_params_file =
          match params_content with
          | No_parameters -> no_parameters_json
          | Has_parameters entire_params_file ->
              (* Describe the parameter set *)
              let ps_description = Ezjsonm.value_to_string param_set in
              let ps_description_l =
                String.cuts ~sep:"\n" ps_description
                |> List.map (fun s -> ";   " ^ s)
              in
              let ps_description_commented =
                Printf.sprintf "; Parameter Set =\n%s"
                  (String.concat ~sep:"\n" ps_description_l)
              in
              let ps_comment =
                Comment
                  (Plain_comment ({ row = 0; col = 0 }, ps_description_commented))
              in
              Queue.add ps_comment pending_prints;
              entire_params_file
        in
        (* Print Dune stanzas *)
        let f_stanza sexpf =
          let sexps =
            sexpf
              { entire_params_file; params = validated_param_set; params_idx }
          in
          List.iter (fun sexp -> Queue.add (Sexp sexp) pending_prints) sexps
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
  render (json_from_argv ()) plain_hum_config stanza_sexpf_lst

let pretty (stanza_sexpf_lst : (args -> out) list) =
  render (json_from_argv ()) pretty stanza_sexpf_lst

let plain_hum_with_params params (stanza_sexpf_lst : (args -> out) list) =
  render (Has_parameters params) plain_hum_config stanza_sexpf_lst
