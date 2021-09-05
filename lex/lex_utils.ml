(*
  use: cat <script.ml> | [ GWREPL_VERBOSE=1 ] [ GWREPL_FORCE_UNPACK=1 ]
   [ GWREPL_NOPROMPT=1 ] gwrepl.exe [script_arg1] ...
*)

(**/**) (* Utils. *)

let skip_to_next_message ic =
  let rec loop () =
    let line = input_line ic in
    if Mutil.start_with "    " 0 line then line else loop ()
  in loop ()
in

let get_all_versions ic =
  let rec loop accu =
    let line = try input_line ic with End_of_file -> "" in
    if line = "" then accu
    else
      try
        let i = String.index line ':' in
        let lang = String.sub line 0 i in
        let transl = String.sub line (i + 1) (String.length line - i - 1) in
        if lang <> "->" then loop ((lang, transl) :: accu) else [("alias", "")]
      with Not_found -> accu
  in loop []
in

(* Missing or unused translation. *)

let get_ml_files repo =
  Files.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".ml")
in

let get_tpl_files repo =
  Files.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".txt")
in

(* Récupère tous les identifiants de message de lexicon. *)
let get_lexicon_msg lexicon =
  let ic = open_in lexicon in
  let rec loop acc = match skip_to_next_message ic with
    | exception End_of_file -> close_in ic ; List.rev acc
    | msg -> loop (String.sub msg 4 (String.length msg - 4) :: acc)
  in loop []
in

let cut_all_msg_src acc s =
  let regexp = Str.regexp "transl" in
  let rec loop i acc =
    try
      let i = Str.search_forward regexp s i in
      let start = String.index_from s i '"' in
      let stop =
        let rec loop k =
          let stop = String.index_from s k '"' in
          if s.[stop - 1] = '\\' then loop (stop + 1)
          else stop
        in loop (start + 1)
      in
      loop (stop + 1) (String.sub s (start + 1) (stop - start - 1) :: acc)
    with Not_found -> acc
  in loop 0 acc
in

let get_msg_src repo =
  (* TODO the current setup misses translations with the string on the next line !! *)
  let regexp = Str.regexp "transl.* \"" in
  let _ = "\"" in (* just for quotes balancing in BBedit. Putting it in comment fails!! *)
  List.fold_left begin fun acc dir ->
    List.fold_left begin fun acc src ->
      let ic = open_in src in
      let rec loop acc =
        match input_line ic with
        | exception End_of_file -> close_in ic ; acc
        | line ->
          if Str.string_match regexp line 0
          then cut_all_msg_src acc line
          else acc
      in loop acc
    end acc (get_ml_files dir)
  end [] repo
in

let cut_all_msg acc s =
  let rec loop i acc =
    try
      let start = String.index_from s i '[' in
      let stop = String.index_from s (start + 1) ']' in
      let w =
        if s.[start + 1] = '*'
        then String.sub s (start + 2) (stop - start - 2)
        else String.sub s (start + 1) (stop - start - 1)
      in
      let w =
        match String.index_opt w ':' with
        | Some i ->
          if (i + 2) < String.length w && w.[i + 1] = ':' && w.[i + 2] = ':'
          then String.sub w 0 i
          else w
        | None -> w
      in
      let multi_msg w =
        match String.index_opt w ':' with
        | Some i ->
          if (i + 1) < String.length w && w.[i + 1] = ':'
          then (String.sub w 0 i) :: (String.sub w (i+2) (String.length w - i - 2)) :: acc
          else w :: acc
        | None -> w :: acc
      in
      let acc =
        if not (Mutil.start_with "type=" 0 w)
        && not (Mutil.start_with "value=" 0 w)
        && not (Mutil.start_with "name=" 0 w)
        && not (Mutil.start_with "id=" 0 w)
        then multi_msg w
        else acc
      in
      loop (stop + 1) acc
    with Not_found -> acc
  in loop 0 acc
in

let get_msg_tpl repo : string list =
  let regexp = Str.regexp "[*?[a-z]+]" in
  List.fold_left begin fun acc dir ->
    List.fold_left begin fun acc tpl ->
      let ic = open_in tpl in
      let rec loop acc = match input_line ic with
        | exception End_of_file -> close_in ic ; acc
        | line ->
          if Str.string_match regexp line 0
          then loop (cut_all_msg acc line)
          else loop acc
      in loop acc
    end acc (get_tpl_files dir)
  end [] repo
in

let module StringSet = Set.Make (String) in

(* Essaie de chercher tous les identifiants de message du répository et *)
(* recherche s'il ne sont plus utilisés pour au contraire non traduit.  *)
let missing_or_unused_msg lexicon repo log =
  let absolute f =
    if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f
  in
  let lexicon = absolute lexicon in
  let repo = absolute repo in
  let repo_src = Filename.concat repo "lib" in
  let repo_bin = Filename.concat repo "bin" in
  let repo_tpl = Filename.concat repo (Filename.concat "hd" "etc") in
  let repo_plugins = Filename.concat repo "plugins" in
  (* TODO, scan plugin lex as well *)
  let lex = get_lexicon_msg lexicon in
  let msg_src = get_msg_src [repo_src; repo_bin; repo_plugins] in
  let msg_tpl = get_msg_tpl [repo_tpl; repo_plugins] in
  let msg =
    List.fold_left (fun acc e -> StringSet.add e acc) StringSet.empty (List.rev_append msg_src msg_tpl)
    |> StringSet.elements
    |> List.sort (fun x y -> Stdlib.compare (String.lowercase_ascii x) (String.lowercase_ascii y))
  in
  if log then begin
    (match try Some (open_out "log_lex") with Sys_error _ -> None with
       | Some oc ->
         List.iter (fun w -> Printf.fprintf oc "%s\n" w) lex;
         close_out oc
       | None -> ());
    (match try Some (open_out "log_msg") with Sys_error _ -> None with
       | Some oc ->
         List.iter (fun w -> Printf.fprintf oc "%s\n" w) msg;
         close_out oc
       | None -> ());
    print_endline
      "View log_lex for lexicon msg and log_msg for src and tpl msg."
  end
  else begin
    Printf.fprintf stdout "\nMessage in lexicon not used anymore in %s and %s:\n%!" repo repo_tpl;
    let lex_cnt = ref 0 in
    List.iter (fun w -> if not (List.mem w msg) then begin print_endline w; incr lex_cnt end) lex;
    Printf.fprintf stdout "\nMessage from %s and %s not in lexicon:\n%!" repo repo_tpl;
    let msg_cnt = ref 0 in
    List.iter (fun w -> if not (List.mem w lex) then begin print_endline w; incr msg_cnt end) msg;
    Printf.fprintf stdout "\n%d messages in sources, %d messages in lexicon\n" (List.length msg) (List.length lex);
    Printf.fprintf stdout "%d messages not used, %d messages not translated\n" !lex_cnt !msg_cnt;
  end
in

(* Missing translation. *)

let missing_languages list languages =
  List.fold_left (fun acc lang ->
    if not (List.mem_assoc lang list) then lang :: acc else acc) [] languages
in

let print_transl_en_fr list =
  let en_transl = try List.assoc "en" list with Not_found -> "" in
  let fr_transl = try List.assoc "fr" list with Not_found -> "" in
  if en_transl <> "" then print_endline ("en:" ^ en_transl);
  if fr_transl <> "" then print_endline ("fr:" ^ fr_transl)
in

let missing_translation lexicon languages =
  let ic = open_in lexicon in
  let rec loop () = match skip_to_next_message ic with
    | exception End_of_file -> close_in ic
    | msg ->
      let list = get_all_versions ic in
      if list = [("alias", "")] then loop ()
      else (
        let list' = missing_languages list languages in
        if list' <> [] then (
          print_endline msg;
          print_transl_en_fr list;
          List.iter (fun lang -> print_endline (lang ^ ":")) (List.rev list') ;
          print_string "\n"; loop ()))
  in loop ()
in

(* Sorting. *)

let module Lex_map = Map.Make
    (struct
      type t = string
      let compare x y =
        compare (String.lowercase_ascii x) (String.lowercase_ascii y)
    end)
in

let merge = ref false in
let first = ref false in

let sort_lexicon lexicon =
  let lex_sort = ref Lex_map.empty in
  (match try Some (open_in lexicon) with Sys_error _ -> None with
     | Some ic ->
       (try
          while true do
            let msg = skip_to_next_message ic in
            let list = get_all_versions ic in
            let list' = List.sort (fun (x, _) (y, _) -> compare x y) list in
            let list' = if !merge then match Lex_map.find_opt msg !lex_sort with
                | Some list ->
                  (* merge list and list' *)
                  let list' =
                    let rec loop accu list =
                      match list with
                      | [] -> accu
                      | (k, v):: list -> loop ((k, v) :: accu) list
                    in loop list' list
                  in
                  if !first then
                    List.sort_uniq (fun (x, _) (y, _) -> compare x y) list'
                  else
                    List.sort_uniq (fun (x, _) (y, _) -> compare x y) (List.rev list')
                | None -> list'
              else list'
            in
            lex_sort := Lex_map.add msg list' !lex_sort
          done
        with End_of_file -> ());
       close_in ic
     | None -> ());
  Lex_map.iter
    (fun msg list ->
       print_endline msg;
       List.iter
         (fun (lang, transl) -> print_endline (lang ^ ":" ^ transl)) list;
       print_string "\n")
    !lex_sort
in


(* Main. *)

let lang_default =
  [ "af"
  ; "bg"
  ; "br"
  ; "ca"
  ; "co"
  ; "cs"
  ; "da"
  ; "de"
  ; "en"
  ; "eo"
  ; "es"
  ; "et"
  ; "fi"
  ; "fr"
  ; "he"
  ; "is"
  ; "it"
  ; "lv"
  ; "nl"
  ; "no"
  ; "oc"
  ; "pl"
  ; "pt"
  ; "pt-br"
  ; "ro"
  ; "ru"
  ; "sk"
  ; "sl"
  ; "sv"
  ; "tr"
  ; "zh"
  ]
in

let lang = ref lang_default in

let lexicon = ref "" in
let lex_sort = ref false in
let missing = ref false in
let orphans = ref false in
let repo = ref "" in
let log = ref false in

let speclist =
  [ ("-missing", Arg.Set missing
    ," Print missing translation for these lang: " ^ String.concat "," lang_default ^".")
  ; ("-missing-lang", Arg.String (fun s -> missing := true ; lang := String.split_on_char ',' s)
    ," Same as -missing, but use a comma-separated list of lang instead of the default one.")
  ; ("-repo", Arg.String (fun x -> repo := x)
    ," Define repo location. If repo is defined, lexicon is relative to repo")
  ; ("-orphans", Arg.Set orphans
    ," Check missing or unused keyword. -repo must be defined")
  ; ("-log", Arg.Set log, " Option for orphans. Print in log files instead of stdout.")
  ; ("-sort", Arg.Set lex_sort, " Sort the lexicon (both key and content).")
  ; ("-first", Arg.Set first, " If multiple language entries, select first occurence.")
  ; ("-merge", Arg.Set merge, " Merge rather than replace new lexicon entries.")
  ] |> Arg.align
in

let anonfun s =
  repo := if !repo = "" then Filename.dirname Sys.argv.(0) else !repo;
  lexicon := Filename.concat !repo s
in

let usage = "Usage: cat lex_utils.ml |" ^
  Sys.argv.(0) ^ " [options] lexicon (relative to gw)"
in

let main () =
  Arg.parse speclist anonfun usage;
  repo := if !repo = "" then "." else !repo;
  if !lexicon = "" then
    lexicon := String.concat Filename.dir_sep [ !repo; "hd"; "lang"; "lexicon.txt"];
  Printf.eprintf "Running lex_utils.ml on lexicon: %s\n" !lexicon;
  if !orphans && !repo = "" then (Arg.usage speclist usage; exit 2);
  if !lex_sort then sort_lexicon !lexicon
  else if !missing then missing_translation !lexicon !lang
  else if !orphans then missing_or_unused_msg !lexicon !repo !log;
  Printf.eprintf "Done\n"
in

Printexc.print main () ;;
