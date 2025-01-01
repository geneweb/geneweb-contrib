(*
  use: echo "#use \"lex/lex_utils.ml\";;main \"-sort -repo ../geneweb\";;" | utop -stdin
*)

#require "geneweb.gwdb_driver";;
#require "geneweb.gwdb-legacy";;
#require "geneweb.sosa";;
#require "geneweb.sosa_zarith";;
#require "geneweb";;

open Geneweb;;

(**/**) (* Utils. *)

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
;;

let lang = ref lang_default;;

let lexicon = ref "";;
let lex_sort = ref false;;
let missing = ref false;;
let orphans = ref false;;
let print_langs = ref false;;
let out_file = ref "";;
let repo = ref "";;
let log = ref false;;
let current = ref 0;;
let only = ref false;;
let merge = ref false;;
let first = ref false;;
let args = ref [|"-help"|];;


(**/**) (* Utils. *)

let skip_to_next_message ic =
  let rec loop () =
    let line = input_line ic in
    if Mutil.start_with "    " 0 line then line else loop ()
  in loop ()
;;

let get_all_versions ic =
  let rec loop accu =
    let line = try input_line ic with End_of_file -> "" in
    if line = "" then (accu, "")
    else
      try
        let i = String.index line ':' in
        let lang = String.sub line 0 i in
        let transl = String.sub line (i + 1) (String.length line - i - 1) in
        if lang = "->" then (["->", transl], transl)
        else loop ((lang, transl) :: accu)
      with Not_found -> (accu, "")
  in loop []
;;

(**/**) (* Missing or unused translation. *)

let get_ml_files repo =
  Mutil.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".ml")
;;

let get_tpl_files repo =
  Mutil.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".txt")
;;

let get_lex_files repo =
  Mutil.ls_r [repo]
  |> List.filter (fun x ->
    Filename.basename (Filename.dirname x) = "lex" &&
    Filename.check_suffix x ".txt")
;;

(* RŽcupre tous les identifiants de message de lexicon. *)
let get_lexicon_msg lexicon =
  let lex = ref [] in
  match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          lex := msg :: !lex
        done
      with End_of_file -> ());
      close_in ic;
      List.rev_map (fun w -> String.sub w 4 (String.length w - 4)) !lex
  | None -> !lex
;;

let cut_all_msg_src s =
  let list = ref [] in
  let i = ref 0 in
  let regexp = Str.regexp "transl" in
  try
    while true do
      i := Str.search_forward regexp s !i;
      let start = String.index_from s !i '"' in
      let (stop, s) =
        let rec loop k s =
          let stop = String.index_from s k '"' in
          if s.[stop - 1] = '\\' then
            loop (stop + 2)
            ((String.sub s 0 (stop-1)) ^ (String.sub s (stop) ((String.length s) - stop)))
          else (stop, s)
        in
        loop (start + 1) s
      in
      list := String.sub s (start + 1) (stop - start - 1) :: !list;
      i := stop + 1
    done;
    !list
  with Not_found -> !list
;;

let get_msg_src repl =
  let msg = ref [] in
  let regexp = Str.regexp "transl.* \"" in
  let _ = " \" " in
  List.iter (fun repo ->
    List.iter
      (fun src ->
        match try Some (open_in src) with Sys_error _ -> None with
        | Some ic ->
            (try
              while true do
                let line = input_line ic in
                let has_msg =
                  try
                    ignore (Str.search_forward regexp line 0);
                    true
                  with Not_found -> false
                in
                if has_msg then msg := line :: !msg
                else ()
              done
            with End_of_file -> ());
            close_in ic;
        | None -> ())
      (get_ml_files repo))
    repl;
  List.fold_left
    (fun accu msg -> List.rev_append (cut_all_msg_src msg) accu)
    [] !msg
;;

let cut_all_msg s =
  let list = ref [] in
  let i = ref 0 in
  try
    while true do
      let start = String.index_from s !i '[' in
      let stop = String.index_from s (start + 1) ']' in
      let w =
        if s.[start + 1] = '[' then
          if s.[start + 2] = '*' then
            String.sub s (start + 3) (stop - start - 3)
          else
            String.sub s (start + 2) (stop - start - 2)
        else
          if s.[start + 1] = '*' then
            String.sub s (start + 2) (stop - start - 2)
          else
            String.sub s (start + 1) (stop - start - 1)
      in
      let w =
        try
          (* loop si msg contient ':' *)
          let i = String.index w ':' in
          if (i + 2) < String.length w && w.[i + 1] = ':' && w.[i + 2] = ':'
          then String.sub w 0 i
          else w
        with Not_found -> w
      in
      let multi_msg w =
        try
          let i = String.index w ':' in
          if (i + 1) < String.length w && w.[i + 1] = ':' then
            list := (String.sub w 0 i) :: (String.sub w (i+2) (String.length w - i - 2)) :: !list
          else list := w :: !list
        with Not_found -> list := w :: !list
      in
      let not_msg =
        List.exists
          (fun x -> Mutil.start_with x 0 w)
          ["type="; "value="; "name="; "id="]
      in
      if not_msg then ()
      else multi_msg w;
      i := stop + 1
    done;
    !list
  with Not_found -> !list
;;

let get_msg_tpl repl =
  let msg = ref [] in
  let regexp = Str.regexp "[*?[a-z]+]" in
  List.iter (fun repo ->
    List.iter
      (fun tpl ->
        match try Some (open_in tpl) with Sys_error _ -> None with
        | Some ic ->
            (try
              while true do
                let line = input_line ic in
                let has_msg =
                  try
                    ignore (Str.search_forward regexp line 0);
                    true
                  with Not_found -> false
                in
                if has_msg then msg := line :: !msg
                else ()
              done
            with End_of_file -> ());
            close_in ic;
        | None -> ())
      (get_tpl_files repo))
    repl;
  List.fold_left
    (fun accu msg -> List.rev_append (cut_all_msg msg) accu)
    [] !msg
;;

module StringSet = Set.Make (String);;

(* Essaie de chercher tous les identifiants de message du rŽpository et *)
(* recherche s'il ne sont plus utilisŽs pour au contraire non trdauit.  *)
let missing_or_unused_msg oc repo lexicon log =
  let lexicon =
    if Filename.is_relative lexicon then
      Filename.concat (Sys.getcwd ()) lexicon
    else lexicon
  in
  let repo =
    if Filename.is_relative repo then
      Filename.concat (Sys.getcwd ()) repo
    else repo
  in
  let repo_lib = Filename.concat repo "lib" in
  let repo_bin = Filename.concat repo "bin" in
  let repo_tpl = List.fold_left Filename.concat repo ["hd"; "etc"] in
  let repo_plugins = Filename.concat repo "plugins" in
  let lex = get_lexicon_msg lexicon in
  let lex_files =
    if !only then []
    else get_lex_files repo_plugins
  in
  let lex =
    if lex_files = [] then lex
    else
      let rec loop acc fl =
        match fl with
        | [] -> acc
        | f :: fl ->
          loop (List.append acc (get_lexicon_msg f)) fl
      in loop lex lex_files
  in
  let lex = List.sort
    (fun x y -> Stdlib.compare (String.lowercase_ascii x) (String.lowercase_ascii y)) lex
  in
  let msg_src = get_msg_src [repo_lib; repo_bin; repo_plugins] in
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
    Printf.fprintf oc
      "View log_lex for lexicon msg and log_msg for src and tpl msg."
  end
  else begin
    Printf.fprintf oc
      "\nMessage in lexicon not used anymore in %s (lib, bin, hd/etc, plugins):\n" repo;
    let lex_cnt = ref 0 in
    List.iter
      (fun w ->
        if List.mem w msg then ()
        else begin Printf.fprintf oc "%s\n" w; incr lex_cnt end)
      lex;
    Printf.fprintf oc
      "\nMessage from sources %s (lib, bin, hd/etc, plugins) not in lexicon:\n" repo;
    let msg_cnt = ref 0 in
    List.iter
      (fun w ->
        if List.mem w lex then ()
        else begin Printf.fprintf oc "%s\n" w; incr msg_cnt end)
      msg;
    Printf.fprintf oc
      "\n%d messages in sources, %d messages in lexicon\n" 
        (List.length msg) (List.length lex);
    Printf.fprintf oc
      "%d messages not used, %d messages not translated\n" !lex_cnt !msg_cnt;
  end
;;

(**/**) (* Missing translation. *)

let missing_languages list languages =
  List.fold_left
    (fun accu lang ->
       if not (List.mem_assoc lang list) then (lang :: accu)
       else accu)
    [] languages
;;

let print_transl_en_fr oc list =
  let en_transl = try List.assoc "en" list with Not_found -> "" in
  let fr_transl = try List.assoc "fr" list with Not_found -> "" in
  if en_transl <> "" then Printf.fprintf oc "%s\n" ("en:" ^ en_transl);
  if fr_transl <> "" then Printf.fprintf oc "%s\n" ("fr:" ^ fr_transl)
;;

let missing_translation oc repo lexicon languages =
  let lexicon =
    if Filename.is_relative lexicon then
      Filename.concat (Sys.getcwd ()) lexicon
    else lexicon
  in
  let repo =
    if Filename.is_relative repo then
      Filename.concat (Sys.getcwd ()) repo
    else repo
  in
  let one_lex lexicon =
    Printf.fprintf stdout "\n**** Missing translations for %s\n" lexicon;
    match try Some (open_in lexicon) with Sys_error _ -> None with
    | Some ic ->
        (try
          while true do
            let msg = skip_to_next_message ic in
            let (list, alias) = get_all_versions ic in
            let list' = missing_languages list languages in
            if list' <> [] && alias = "" then
              begin
                Printf.fprintf oc "%s\n" msg;
                print_transl_en_fr oc list;
                List.iter
                  (fun lang -> Printf.fprintf oc "%s\n" (lang ^ ":")) (List.rev list');
                Printf.fprintf oc "\n"
              end
          done
        with End_of_file -> ());
        close_in ic
    | None -> ()
  in
  one_lex lexicon;
  if !only then ()
  else
    let repo_plugins = Filename.concat repo "plugins" in
    let lex_files =
      if !only then []
      else get_lex_files repo_plugins
    in
    List.iter (fun f -> one_lex f) lex_files
;;

(**/**) (* Sorting. *)

module Lex_map = Map.Make
  (struct
    type t = string
    let compare x y =
      compare (String.lowercase_ascii x) (String.lowercase_ascii y)
   end)
;;

let sort_lexicon oc lexicon =
  let lex_sort = ref Lex_map.empty in
  (match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          let (list, alias) = get_all_versions ic in
          let alias = 
            if String.length alias > 1 then
              String.sub alias 1 (String.length alias - 1)
            else alias
          in
          let list' = List.sort (fun (x, _) (y, _) -> compare x y) list in
          let msg =
            if alias = "" then msg
            else ("    " ^ alias ^ msg)
          in
          lex_sort := Lex_map.add msg (alias, list') !lex_sort
        done
      with End_of_file -> ());
      close_in ic
  | None -> ());
  Lex_map.iter
    (fun msg (alias, list) ->
      let msg =
        if alias = "" then msg
        else (String.sub msg (String.length alias + 4)
          (String.length msg - (String.length alias) - 4))
      in
      Printf.fprintf oc "%s\n" msg;
      List.iter
        (fun (lang, transl) -> Printf.fprintf oc "%s\n" (lang ^ ":" ^ transl)) list;
      Printf.fprintf oc "\n")
    !lex_sort
;;

(**/**) (* Main. *)

let speclist = ref
  [ ("-missing", Arg.Set missing
    ," Print missing translation for the set of default langs.
                 Use -langs to see this list.")
  ; ("-langs", Arg.Set print_langs
    ," Prints the list of available langs.")
  ; ("-missing-lang", Arg.String (fun s -> missing := true ; lang := String.split_on_char ',' s)
    ," Same as -missing, but use a comma-separated list of lang
                 instead of the default one.")
  ; ("-repo", Arg.String (fun x -> repo := x)
    ," Define repo location. If repo is defined, lexicon is relative to repo")
  ; ("-orphans", Arg.Set orphans
    ," Check missing or unused keyword. -repo must be defined")
  ; ("-log", Arg.Set log
    ," Assumes -orphans. Print whole content of lexicon in log_lex and of
                 source/templates messages in log_msg.")
  ; ("-o", Arg.String (fun x -> out_file := x)
    ," Prints results to the designated output file.")
  ; ("-sort", Arg.Set lex_sort, " Sort the lexicon (both key and content).")
  ; ("-first", Arg.Set first, " When sorting, if multiple language entries,
                 select first occurence (default is second).")
  ; ("-merge", Arg.Set merge
    , " When sorting, merge rather than replace new
                 lexicon entries (default is replace).")
  ; ("-only", Arg.Set only
    , " Process only designated lexicon.
                 Default is scan plugins for additional lexicon files.")
  ]
;;

let anonfun s = 
  lexicon := if !repo = "" then s else (Filename.concat !repo s)
;;

let usage =
{|Usage:
  cd geneweb_repo
  dune utop
  #use "../geneweb-contrib/lex/lex_utils.ml";;
  #main "options";; (example: "-repo . -missing-lang de")
  
  Options:|};;
;;

let main str =
  let str_l = String.split_on_char ' ' str in
  let str_l = "lex_utils.ml" :: str_l in
  args := Array.of_list (str_l);
  flush stderr;
  (try
    Arg.parse_and_expand_argv_dynamic current args speclist anonfun usage
  with
  | Arg.Help _ -> (Arg.usage (!speclist |> Arg.align) usage; exit 2)
  | Arg.Bad _ -> (Arg.usage (!speclist |> Arg.align) usage;exit 2));
  flush stderr;
  repo := if !repo = "" then "." else !repo;
  if !lexicon = "" then
    lexicon := String.concat Filename.dir_sep [ !repo; "hd"; "lang"; "lexicon.txt"];
  Printf.fprintf stdout "Running lex_utils-2.ml on lexicon: %s%s" !lexicon
    (if !out_file <> "" then Format.sprintf " to %s\n" !out_file else "\n");
  if !print_langs then (
    Printf.fprintf stdout "Available langs: %s\n" (String.concat ", " lang_default));
  if !orphans && !repo = "" then (Arg.usage (!speclist |> Arg.align) usage)
  else (
    if !out_file <> "" then
      match try Some (open_out !out_file) with Sys_error _ -> None with
      | Some oc -> (
          if !lex_sort then sort_lexicon oc !lexicon
          else if !missing then missing_translation oc !repo !lexicon !lang
          else if !orphans then missing_or_unused_msg oc !repo !lexicon !log;
          Printf.eprintf "Done\n")
      | None -> Printf.eprintf "Failed to open out_file\n"
    else (
      if !lex_sort then sort_lexicon stdout !lexicon
      else if !missing then missing_translation stdout !repo !lexicon !lang
      else if !orphans then missing_or_unused_msg stdout !repo !lexicon !log;
      Printf.fprintf stdout "Done\n"))
;;

