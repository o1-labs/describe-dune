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

type dune_unit =
  { name : string
  ; public_name : string option
  ; package : string option
  ; implements : string option
  ; default_implementation : string option
  ; deps : string list
  }

let str x = `String x

let unit_to_json
    ( { name; public_name; package; implements; default_implementation; deps }
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
  in
  `Assoc res

type dune =
  { units : (dune_unit * [ `Executable | `Library | `Test ]) list
  ; file_deps : string list
  ; file_outs : string list
  ; include_subdirs : string option
  }

let is_empty_dune = function
  | { units = []; file_deps = []; file_outs = []; include_subdirs = None } ->
      true
  | _ ->
      false

let empty_dune =
  { units = []; file_deps = []; file_outs = []; include_subdirs = None }

let merge_dune a b =
  { units = a.units @ b.units
  ; file_deps = a.file_deps @ b.file_deps
  ; file_outs = a.file_outs @ b.file_outs
  ; include_subdirs = Option.first_some a.include_subdirs b.include_subdirs
  }

type dune_info = { dune : dune; src : string; subdirs : string list }

let dune_to_json { units; file_deps; file_outs; include_subdirs } =
  [ ("units", `List (List.map ~f:unit_to_json units))
  ; ("file_deps", `List (List.map ~f:str file_deps))
  ; ("file_outs", `List (List.map ~f:str file_outs))
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
  | "preprocessor_deps" ->
      ([], List.concat_map ~f:parse_file_dependency args)
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
      ([], include_dirs)
  | _ ->
      ([], [])

let extract_unit_deps args =
  List.fold_left ~init:([], []) args ~f:(fun (ds, fs) -> function
    | Sexp.List (Atom name :: args') ->
        let ds', fs' = extract_unit_subdeps name args' in
        (ds @ ds', fs @ fs')
    | _ ->
        (ds, fs) )

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
    let deps, file_deps = extract_unit_deps args in
    { units =
        [ ( { name
            ; public_name
            ; package
            ; deps
            ; implements
            ; default_implementation
            }
          , type_ )
        ]
    ; file_deps
    ; file_outs = []
    ; include_subdirs = None
    }

let process_executables type_ args =
  let package = find_single_value_opt "package" args in
  let names = find_multi_value "names" args in
  let public_names = find_multi_value_opt "public_names" args in
  let deps, file_deps = extract_unit_deps args in
  let mk_unit name =
    ( { public_name = None
      ; name
      ; package
      ; deps
      ; implements = None
      ; default_implementation = None
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
  { units; file_deps; file_outs = []; include_subdirs = None }

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

let normalize_dune ~dir { units; file_deps; file_outs; include_subdirs } =
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
  let file_outs' = List.map ~f file_outs in
  let dedup = List.dedup_and_sort ~compare:String.compare in
  { units
  ; file_deps = dedup file_deps'
  ; file_outs = dedup file_outs'
  ; include_subdirs
  }

let extract_rule_subdeps name args =
  match name with
  | "targets" | "target" ->
      (multi_value_args args, [])
  | "deps" ->
      ([], List.concat_map ~f:parse_file_dependency args)
  | _ ->
      ([], [])

let extract_rule_deps args =
  List.fold_left ~init:([], []) args ~f:(fun (os, ds) -> function
    | Sexp.List (Atom name :: args') ->
        let os', ds' = extract_rule_subdeps name args' in
        (os @ os', ds @ ds')
    | _ ->
        (os, ds) )

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
      { units = []; file_deps; file_outs; include_subdirs = None }
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
      ; file_outs = List.map ~f processed.file_outs
      }
  | "include_subdirs" ->
      let value =
        match args with
        | [ Sexp.Atom s ] ->
            s
        | _ ->
            failwith "invalid include_subdirs stanza"
      in
      { units = []
      ; file_deps = []
      ; file_outs = []
      ; include_subdirs = Some value
      }
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
    if dir == "." then Fn.id else String.chop_prefix_exn ~prefix:(dir ^ "/")
  in
  let continue_f sub_result =
    match dune_opt with
    | None ->
        { sub_result with success = sub_result.success && ok }
    | Some dune_ ->
        let dune = { dune_ with file_deps = dune_.file_deps @ ctx } in
        let subdirs =
          List.map ~f:(fun { src; _ } -> chop_prefix src) sub_result.children
        in
        { children = [ { src = dir; dune; subdirs } ]
        ; descendants = sub_result.children @ sub_result.descendants
        ; success = ok || sub_result.success
        }
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
  List.filter_map dir_contents ~f:(fun f ->
      let f' =
        if String.equal dir "." then f else Stdlib.Filename.concat dir f
      in
      try
        if Stdlib.Sys.is_directory f' then Some (process_dir ctx' f') else None
      with _ -> None )
  |> merge_pd_results |> continue_f

let run () =
  let { children; descendants; success } = process_dir [] "." in
  Yojson.Basic.to_channel Stdio.stdout (result_to_json (children @ descendants)) ;
  if not success then Stdlib.exit 10

let cmd = Cmdliner.Term.(pure run $ pure ())

let main_cmd_info =
  Cmdliner.Term.info "describe-dune" ~version:"0.1"
    ~doc:"A command-line utility to turn opam file format to json" ~man:[]

let () = Cmdliner.Term.eval (cmd, main_cmd_info) |> Cmdliner.Term.exit
