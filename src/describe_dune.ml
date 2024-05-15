open Parsexp
open Stdio
open Yojson
open Yojson.Basic.Util
open Base

let fatal_exn = function Stdlib.Sys.Break as e -> raise e | _ -> ()

let load_exn ~filename =
  let open Stdio in
  In_channel.with_file filename ~f:(fun ic ->
      let buf = Bytes.create 8192 in
      let state = Many_and_positions.State.create () in
      let rec loop stack =
        match In_channel.input ic ~buf ~pos:0 ~len:(Bytes.length buf) with
        | 0 ->
            Many_and_positions.feed_eoi state stack
        | n ->
            loop
              (Many_and_positions.feed_subbytes state buf stack ~pos:0 ~len:n)
      in
      loop Many_and_positions.Stack.empty )

type modules =
  { with_standard : bool; include_ : string list; exclude : string list }

type dune_unit =
  { name : string
  ; public_name : string option
  ; package : string option
  ; implements : string option
  ; default_implementation : string option
  ; deps : string list
  ; modules : modules option
  ; has_inline_tests : bool
  }

let str x = `String x

let modules_to_json_fields { with_standard; include_; exclude } =
  [ ("with_standard", `Bool with_standard)
  ; ("include", `List (List.map ~f:str include_))
  ; ("exclude", `List (List.map ~f:str exclude))
  ]

let unit_to_json
    ( { name
      ; public_name
      ; package
      ; implements
      ; default_implementation
      ; deps
      ; modules
      ; has_inline_tests
      }
    , type_ ) =
  let type_json =
    match type_ with
    | `Executable ->
        "exe"
    | `Library ->
        "lib"
    | `Test ->
        "test"
  in
  let res =
    [ ("name", `String name)
    ; ("type", `String type_json)
    ; ("deps", `List (List.map ~f:str deps))
    ]
    @ Option.value_map ~default:[]
        ~f:(fun v -> [ ("public_name", `String v) ])
        public_name
    @ Option.value_map ~default:[]
        ~f:(fun v -> [ ("implements", `String v) ])
        implements
    @ Option.value_map ~default:[]
        ~f:(fun v -> [ ("default_implementation", `String v) ])
        default_implementation
    @ Option.value_map ~default:[]
        ~f:(fun v -> [ ("package", `String v) ])
        package
    @ Option.value_map ~default:[]
        ~f:(fun v -> [ ("modules", `Assoc (modules_to_json_fields v)) ])
        modules
    @ if has_inline_tests then [ ("has_inline_tests", `Bool true) ] else []
  in
  `Assoc res

type file_out_mode_t = Fallback | Standard | Promote

let choose_file_out_mode =
  let impl = function
    | Fallback, b ->
        b
    | a, Fallback ->
        a
    | Standard, b ->
        b
    | a, Standard ->
        a
    | Promote, Promote ->
        Promote
  in
  fun a b -> impl (a, b)

let parse_file_out_mode = function
  | "standard" ->
      Standard
  | "fallback" ->
      Fallback
  | "promote" ->
      Promote
  | s ->
      failwith ("Unknown file out mode: " ^ s)

let file_out_mode_to_json = function
  | Standard ->
      str "standard"
  | Fallback ->
      str "fallback"
  | Promote ->
      str "promote"

module StringMap = Map.M (String)

type file_outs_t = file_out_mode_t StringMap.t

let empty_file_outs = Map.empty (module String)

let merge_file_outs =
  Map.merge_skewed ~combine:(fun ~key:_ -> choose_file_out_mode)

type dune =
  { units : (dune_unit * [ `Executable | `Library | `Test ]) list
  ; file_deps : string list
  ; file_outs : file_outs_t
  ; include_subdirs : string option
  ; has_env : bool
  }

let is_empty_dune = function
  | { units = []
    ; file_deps = []
    ; file_outs
    ; include_subdirs = None
    ; has_env = false
    } ->
      Map.is_empty file_outs
  | _ ->
      false

let empty_dune =
  { units = []
  ; file_deps = []
  ; file_outs = empty_file_outs
  ; include_subdirs = None
  ; has_env = false
  }

let merge_dune a b =
  { units = a.units @ b.units
  ; file_deps = a.file_deps @ b.file_deps
  ; file_outs = merge_file_outs a.file_outs b.file_outs
  ; include_subdirs = Option.first_some a.include_subdirs b.include_subdirs
  ; has_env = a.has_env || b.has_env
  }

type dune_info = { dune : dune; src : string; subdirs : string list }

let dune_to_json { units; file_deps; file_outs; include_subdirs; has_env = _ } =
  [ ("units", `List (List.map ~f:unit_to_json units))
  ; ("file_deps", `List (List.map ~f:str file_deps))
  ; ( "file_outs"
    , `Assoc (Map.to_alist @@ Map.map ~f:file_out_mode_to_json file_outs) )
  ]
  @ Option.value_map include_subdirs ~default:[] ~f:(fun sd ->
        [ ("include_subdirs", `String sd) ] )

let dune_info_to_json { dune; src; subdirs } =
  `Assoc
    ( [ ("src", `String src); ("subdirs", `List (List.map ~f:str subdirs)) ]
    @ dune_to_json dune )

let result_to_json l = `List (List.map ~f:dune_info_to_json l)

let is_stanza name = function
  | Sexp.List (Atom n :: _) ->
      String.equal n name
  | _ ->
      false

let find_stanza name = List.find_exn ~f:(is_stanza name)

let find_stanza_opt name args =
  try Some (List.find_exn ~f:(is_stanza name) args) with Not_found_s _ -> None

let single_value = function
  | Sexp.List [ Atom _; Atom v ] ->
      v
  | _ ->
      failwith "not a single value stanza"

let multi_value_args =
  List.map ~f:(function
    | Sexp.Atom s ->
        s
    | _ ->
        failwith "not a multi value stanza" )

let multi_value = function
  | Sexp.List (Atom _ :: rest) ->
      multi_value_args rest
  | _ ->
      failwith "not a multi value stanza"

let find_single_value name = Fn.compose single_value (find_stanza name)

let find_single_value_opt name =
  Fn.compose (Option.map ~f:single_value) (find_stanza_opt name)

let find_multi_value name = Fn.compose multi_value (find_stanza name)

let find_multi_value_opt name =
  Fn.compose (Option.map ~f:multi_value) (find_stanza_opt name)

let rec parse_file_dependency = function
  | Sexp.Atom f
  | List [ Atom "file"; Atom f ]
  (* TODO read include file for dependencies *)
  | List [ Atom "include"; Atom f ]
  | List [ Atom "source_tree"; Atom f ] ->
      [ f ]
  | List (Atom s :: args) when String.is_prefix ~prefix:":" s ->
      List.concat_map ~f:parse_file_dependency args
  | _ ->
      []

let libraries =
  List.map ~f:(function
    | Sexp.Atom s ->
        s
    | List [ Atom "re_export"; Atom s ] ->
        s
    | _ ->
        failwith "not a library list" )

let extract_unit_subdeps name args =
  match name with
  | "libraries" | "ppx_runtime_libraries" ->
      (libraries args, [])
  | "instrumentation" ->
      let backend = find_multi_value "backend" args in
      ([ List.hd_exn backend ], [])
  | "preprocess" ->
      let pps =
        find_multi_value_opt "pps" args
        |> Option.value ~default:[]
        |> List.take_while ~f:(Fn.compose not @@ String.equal "--")
        |> List.filter ~f:(Fn.compose not @@ String.is_prefix ~prefix:"-")
      in
      (pps, [])
  | "deps" ->
      ([], List.concat_map ~f:parse_file_dependency args)
  | "link_deps" ->
      ([], List.concat_map ~f:parse_file_dependency args)
  | "preprocessor_deps" ->
      ([], List.concat_map ~f:parse_file_dependency args)
  | "js_of_ocaml" ->
      let files = find_multi_value_opt "javascript_files" args in
      ([], Option.value ~default:[] files)
  | "inline_tests" ->
      let deps =
        find_stanza_opt "deps" args
        |> Option.bind ~f:(function
             | Sexp.List (_ :: rest) ->
                 Some rest
             | _ ->
                 None )
        |> Option.value ~default:[]
      in
      ([], List.concat_map ~f:parse_file_dependency deps)
  | "foreign_stubs" ->
      let include_dirs =
        find_stanza_opt "include_dirs" args
        |> Option.bind ~f:(function
             | Sexp.List (_ :: rest) ->
                 Some rest
             | _ ->
                 None )
        |> Option.value ~default:[]
        |> List.filter_map ~f:(function Sexp.Atom s -> Some s | _ -> None)
      in
      let language_opt = find_single_value_opt "language" args in
      let names =
        find_multi_value_opt "names" args |> Option.value ~default:[]
      in
      let fnames language =
        if String.equal language "c" then fun n -> [ n ^ ".c" ]
        else if String.equal language "cxx" then fun n ->
          [ n ^ ".cpp"; n ^ ".cxx"; n ^ ".cc" ]
        else Fn.const []
      in
      let c_files =
        Option.value_map ~default:[] language_opt ~f:(fun l ->
            List.concat_map ~f:(fnames l) names )
      in
      ([], include_dirs @ c_files)
  | _ ->
      ([], [])

let extract_unit_deps args =
  List.fold_left ~init:([], []) args ~f:(fun (ds, fs) -> function
    | Sexp.List (Atom name :: args') ->
        let ds', fs' = extract_unit_subdeps name args' in
        (ds @ ds', fs @ fs')
    | _ ->
        (ds, fs) )

let rec parse_module_list = function
  | Sexp.Atom ":standard" ->
      (true, [])
  | Atom "\\" ->
      failwith "exclusion is only supported on the top-level of modules"
  | Atom s when String.is_prefix ~prefix:":" s ->
      failwith ("Unsupported atom " ^ s ^ "in modules stanza")
  | Atom s ->
      (false, [ s ])
  | List s ->
      List.fold_left ~init:(false, []) s
        ~f:(fun (with_standard, modules) sexp ->
          let with_standard', modules' = parse_module_list sexp in
          (with_standard' || with_standard, modules @ modules') )

let rec parse_modules_toplevel = function
  | [ Sexp.List ls ] ->
      parse_modules_toplevel ls
  | [ s; Atom "\\"; t ] ->
      let with_standard, include_ = parse_module_list s in
      let with_standard', exclude = parse_module_list t in
      if with_standard' then failwith "Do not support :standard in exclude list"
      else { with_standard; include_; exclude }
  | ls ->
      let with_standard, include_ = parse_module_list (Sexp.List ls) in
      { with_standard; include_; exclude = [] }

let parse_modules_opt name args =
  find_stanza_opt name args
  |> Option.map ~f:(function
       | Sexp.List (_ :: ls) ->
           parse_modules_toplevel ls
       | _ ->
           failwith "Unsupported modules stanza" )

let process_unit type_ args =
  (* Allow to permanently disable units, as a hacky debugging method *)
  let is_disabled =
    find_stanza_opt "enabled_if" args
    |> Option.value_map ~default:false ~f:(function
         | Sexp.List (_ :: Atom "false" :: _) ->
             true
         | _ ->
             false )
  in
  if is_disabled then empty_dune
  else
    let name = find_single_value "name" args in
    let public_name = find_single_value_opt "public_name" args in
    let package = find_single_value_opt "package" args in
    let default_implementation =
      find_single_value_opt "default_implementation" args
    in
    let implements = find_single_value_opt "implements" args in
    let modules = parse_modules_opt "modules" args in
    let has_inline_tests =
      find_stanza_opt "inline_tests" args |> Option.is_some
    in
    let deps, file_deps = extract_unit_deps args in
    { units =
        [ ( { name
            ; public_name
            ; package
            ; deps
            ; implements
            ; default_implementation
            ; modules
            ; has_inline_tests
            }
          , type_ )
        ]
    ; file_deps
    ; file_outs = empty_file_outs
    ; include_subdirs = None
    ; has_env = false
    }

let process_executables type_ args =
  let package = find_single_value_opt "package" args in
  let names = find_multi_value "names" args in
  let public_names = find_multi_value_opt "public_names" args in
  let modules = parse_modules_opt "modules" args in
  let has_inline_tests =
    find_stanza_opt "inline_tests" args |> Option.is_some
  in
  let deps, file_deps = extract_unit_deps args in
  let mk_unit name =
    ( { public_name = None
      ; name
      ; package
      ; deps
      ; implements = None
      ; default_implementation = None
      ; modules
      ; has_inline_tests
      }
    , type_ )
  in
  let mk_unit2 name public_name =
    let public_name =
      if String.equal public_name "-" then None else Some public_name
    in
    ( { public_name
      ; name
      ; package
      ; deps
      ; implements = None
      ; default_implementation = None
      ; modules
      ; has_inline_tests
      }
    , type_ )
  in
  let units =
    match public_names with
    | Some pns ->
        List.map2_exn ~f:mk_unit2 names pns
    | None ->
        List.map ~f:mk_unit names
  in
  { units
  ; file_deps
  ; file_outs = empty_file_outs
  ; include_subdirs = None
  ; has_env = false
  }

let filepath_parts =
  let rec loop acc fname =
    match Stdlib.Filename.(dirname fname, basename fname) with
    | ".", "." ->
        acc
    | ("/" as base), "/" ->
        base :: acc
    | rest, dir ->
        loop (dir :: acc) rest
  in
  loop []

let of_filepath_parts = function
  | [] ->
      "."
  | root :: rest ->
      List.fold rest ~init:root ~f:Stdlib.Filename.concat

let normalize_path_parts =
  Fn.compose List.rev
  @@ List.fold_left ~init:[] ~f:(fun acc el ->
         match (el, acc) with
         (* Ignoring . parts in path *)
         | ".", _ ->
             acc
         (* If last element of accumulator is '..', then then the normalized
            path starts with a series of '..' and it's not reducible *)
         | _, ".." :: _ ->
             el :: acc
         (* If last element of accumulator isn't '..' and new element is '..',
            we can drop them both *)
         | "..", _ :: r ->
             r
         (* Otherwise, just add new element to accumulator *)
         | _ ->
             el :: acc )

let map_file_out_keys ~f file_outs =
  Map.to_alist file_outs
  |> List.fold_left
       ~f:(fun m (k, v) ->
         Map.update m (f k)
           ~f:(Option.value_map ~default:v ~f:(choose_file_out_mode v)) )
       ~init:empty_file_outs

let normalize_dune ~dir
    { units; file_deps; file_outs; include_subdirs; has_env } =
  let dir_parts = filepath_parts dir in
  let dir_len = List.length dir_parts in
  let f path =
    let path_parts = filepath_parts path |> normalize_path_parts in
    let non_parented = List.drop_while ~f:(String.equal "..") path_parts in
    let num_parents = List.(length path_parts - length non_parented) in
    let prefix =
      if num_parents > dir_len then
        List.init (num_parents - dir_len) ~f:(Fn.const "..")
      else List.take dir_parts (dir_len - num_parents)
    in
    of_filepath_parts @@ prefix @ non_parented
  in
  let file_deps' = List.map ~f file_deps in
  let file_outs' = map_file_out_keys ~f file_outs in
  let dedup = List.dedup_and_sort ~compare:String.compare in
  { units
  ; file_deps = dedup file_deps'
  ; file_outs = file_outs'
  ; include_subdirs
  ; has_env
  }

let extract_rule_deps args =
  let target_ = find_single_value_opt "target" args in
  let targets_ = find_multi_value_opt "targets" args in
  let mode_ = find_single_value_opt "mode" args in
  let deps =
    find_stanza_opt "deps" args
    |> function
    | Some (Sexp.List (Atom _ :: args)) ->
        List.concat_map ~f:parse_file_dependency args
    | _ ->
        []
  in
  let targets =
    Option.value_map ~default:[] ~f:List.return target_
    @ Option.value ~default:[] targets_
  in
  let mode = Option.value_map ~default:Standard ~f:parse_file_out_mode mode_ in
  let outs =
    List.fold_left
      ~f:
        (Map.update
           ~f:(Option.value_map ~default:mode ~f:(choose_file_out_mode mode)) )
      ~init:empty_file_outs targets
  in
  (outs, deps)

let rec process_sexp' ~dir ~args = function
  | "library" ->
      process_unit `Library args
  | "executable" | "toplevel" ->
      process_unit `Executable args
  | "test" ->
      process_unit `Test args
  | "tests" ->
      process_executables `Test args
  | "executables" ->
      process_executables `Executable args
  | "rule" ->
      let file_outs, file_deps = extract_rule_deps args in
      { empty_dune with file_deps; file_outs }
  | "include" ->
      let file =
        match args with
        | [ Sexp.Atom f ] ->
            f
        | _ ->
            failwith "invalid include stanza"
      in
      let processed =
        process_sexp_file (dir ^ Stdlib.Filename.dir_sep ^ file)
      in
      let f = Stdlib.Filename.(concat @@ dirname file) in
      { processed with
        file_deps = file :: List.map ~f processed.file_deps
      ; file_outs = map_file_out_keys ~f processed.file_outs
      }
  | "include_subdirs" ->
      let value =
        match args with
        | [ Sexp.Atom s ] ->
            s
        | _ ->
            failwith "invalid include_subdirs stanza"
      in
      { empty_dune with include_subdirs = Some value }
  | "env" ->
      { empty_dune with has_env = true }
  | _ ->
      empty_dune

and process_sexp = function
  | Sexp.List (Atom name :: args) ->
      process_sexp' name ~args
  | _ ->
      fun ~dir:_ -> empty_dune

and process_sexp_file filename =
  let dir = Stdlib.Filename.dirname filename in
  List.fold_left ~f:merge_dune ~init:empty_dune
    (Conv_many.conv_exn (load_exn ~filename) (process_sexp ~dir))

type process_dir_result =
  { children : dune_info list; descendants : dune_info list; success : bool }

type process_dir_ctx = string list (* list of files to be added to file_deps *)

let def_pd_result = { children = []; descendants = []; success = true }

let merge_pd_result s t =
  { children = s.children @ t.children
  ; descendants = s.descendants @ t.descendants
  ; success = s.success && t.success
  }

let merge_pd_results = List.fold_left ~init:def_pd_result ~f:merge_pd_result

let rec process_dir : process_dir_ctx -> string -> process_dir_result =
 fun ctx dir ->
  (* TODO read dune-project, accept dune-file if specified *)
  let dune_file = Stdlib.Filename.concat dir "dune" in
  let dune_opt, ok =
    try
      if Stdlib.Sys.file_exists dune_file then
        let dune = process_sexp_file dune_file |> normalize_dune ~dir in
        (Option.some_if (not @@ is_empty_dune dune) dune, true)
      else (None, true)
    with e ->
      fatal_exn e ;
      Stdlib.Printf.eprintf "Error on file %s: %s\n" dune_file (Exn.to_string e) ;
      (None, false)
  in
  let chop_prefix =
    if String.equal dir "." then Fn.id
    else String.chop_prefix_exn ~prefix:(dir ^ "/")
  in
  let dir_contents = Stdlib.Sys.readdir dir |> Array.to_list in
  let ctx' =
    ctx
    @ List.filter_map dir_contents ~f:(fun f ->
          let f' =
            if String.equal dir "." then f else Stdlib.Filename.concat dir f
          in
          Option.some_if
            String.(
              equal f "opam" || equal f "dune-project"
              || is_suffix ~suffix:".opam" f)
            f' )
  in
  let ctx'' =
    ctx'
    @ Option.value_map ~default:[] dune_opt ~f:(fun { has_env; file_deps; _ } ->
          if has_env then dune_file :: file_deps else [] )
  in
  let continue_f sub_result =
    match dune_opt with
    | None ->
        { sub_result with success = sub_result.success && ok }
    | Some dune_ ->
        let dune = { dune_ with file_deps = dune_.file_deps @ ctx' } in
        let subdirs =
          List.map ~f:(fun { src; _ } -> chop_prefix src) sub_result.children
        in
        { children = [ { src = dir; dune; subdirs } ]
        ; descendants = sub_result.children @ sub_result.descendants
        ; success = ok || sub_result.success
        }
  in
  List.filter_map dir_contents ~f:(fun f ->
      let f' =
        if String.equal dir "." then f else Stdlib.Filename.concat dir f
      in
      try
        if Stdlib.Sys.is_directory f' then Some (process_dir ctx'' f') else None
      with _ -> None )
  |> merge_pd_results |> continue_f

let run () =
  let { children; descendants; success } = process_dir [] "." in
  Yojson.Basic.to_channel Stdio.stdout (result_to_json (children @ descendants)) ;
  if not success then Stdlib.exit 10

let cmd = Cmdliner.Term.(const run $ const ())

let main_cmd_info =
  Cmdliner.Term.info "describe-dune" ~version:"0.1"
    ~doc:"A command-line utility to turn opam file format to json" ~man:[]

let () = Cmdliner.Term.eval (cmd, main_cmd_info) |> Cmdliner.Term.exit
