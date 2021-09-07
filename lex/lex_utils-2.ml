
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
in

(**/**) (* Missing or unused translation. *)

let get_ml_files repo =
  Mutil.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".ml")
in

let get_tpl_files repo =
  Mutil.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".txt")
in

let get_lex_files repo =
  Mutil.ls_r [repo]
  |> List.filter (fun x ->
    Filename.basename (Filename.dirname x) = "lex" &&
    Filename.check_suffix x ".txt")
in

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
in

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
in

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
in

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
in

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
in

let module StringSet = Set.Make (String) in

let only = ref false in

(* Essaie de chercher tous les identifiants de message du rŽpository et *)
(* recherche s'il ne sont plus utilisŽs pour au contraire non trdauit.  *)
let missing_or_unused_msg lexicon repo log =
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
    print_endline
      "View log_lex for lexicon msg and log_msg for src and tpl msg."
  end
  else begin
    Printf.fprintf stdout
      "\nMessage in lexicon not used anymore in %s (lib, bin, hd/etc, plugins):\n" repo;
    flush stdout;
    let lex_cnt = ref 0 in
    List.iter
      (fun w ->
        if List.mem w msg then ()
        else begin print_endline w; incr lex_cnt end)
      lex;
    Printf.fprintf stdout
      "\nMessage from sources %s (lib, bin, hd/etc, plugins) not in lexicon:\n" repo;
    flush stdout;
    let msg_cnt = ref 0 in
    List.iter
      (fun w ->
        if List.mem w lex then ()
        else begin print_endline w; incr msg_cnt end)
      msg;
    Printf.fprintf stdout
      "\n%d messages in sources, %d messages in lexicon\n" 
        (List.length msg) (List.length lex);
    Printf.fprintf stdout
      "%d messages not used, %d messages not translated\n" !lex_cnt !msg_cnt;
    flush stdout;
  end
in

(**/**) (* Missing translation. *)

let missing_languages list languages =
  List.fold_left
    (fun accu lang ->
       if not (List.mem_assoc lang list) then (lang :: accu)
       else accu)
    [] languages
in

let print_transl_en_fr list =
  let en_transl = try List.assoc "en" list with Not_found -> "" in
  let fr_transl = try List.assoc "fr" list with Not_found -> "" in
  if en_transl <> "" then print_endline ("en:" ^ en_transl);
  if fr_transl <> "" then print_endline ("fr:" ^ fr_transl)
in

let missing_translation repo lexicon languages =
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
    Printf.printf "\n**** Missing translations for %s\n" lexicon;
    match try Some (open_in lexicon) with Sys_error _ -> None with
    | Some ic ->
        (try
          while true do
            let msg = skip_to_next_message ic in
            let (list, alias) = get_all_versions ic in
            let list' = missing_languages list languages in
            if list' <> [] && alias = "" then
              begin
                print_endline msg;
                print_transl_en_fr list;
                List.iter
                  (fun lang -> print_endline (lang ^ ":")) (List.rev list');
                print_string "\n"
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
in


(**/**) (* Sorting. *)

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
          let (list, alias) = get_all_versions ic in
          Printf.printf "Msg1: %s (%d)\n" msg (List.length list);
          let alias = 
            if String.length alias > 1 then
              String.sub alias 1 (String.length alias - 1)
            else alias
          in
          Printf.printf "Alias: |%s|\n" alias;
          let list' = List.sort (fun (x, _) (y, _) -> compare x y) list in
          let msg =
            if alias = "" then msg
            else ("    " ^ alias ^ msg)
          in
          Printf.printf "Msg2: %s (%d)\n" msg (List.length list');
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
      print_endline msg;
      List.iter
        (fun (lang, transl) -> print_endline (lang ^ ":" ^ transl)) list;
      print_string "\n")
    !lex_sort
in

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


(**/**) (* Main. *)
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
  ; ("-log", Arg.Set log
    ," Assumes -orphans. Print whole content of lexicon in log_lex and of
       source/templates messages in log_msg.")
  ; ("-sort", Arg.Set lex_sort, " Sort the lexicon (both key and content).")
  ; ("-first", Arg.Set first, " When sorting, if multiple language entries,
       select first occurence (default is second).")
  ; ("-merge", Arg.Set merge, " When sorting, merge rather than replace new
       lexicon entries (default is replace).")
  ; ("-only", Arg.Set only, " Process only designated lexicon. 
       Default is scan plugins for additional lexicon files.")
  ] |> Arg.align
in

let anonfun s = lexicon := if !repo = "" then s else (Filename.concat !repo s) in

let usage = "Usage: cat lex_utils.ml | " ^ Sys.argv.(0) ^ " [options] lexicon" in

let main () =
  Arg.parse speclist anonfun usage;
  if !lexicon = "" then (Arg.usage speclist usage; exit 2);
  if !orphans && !repo = "" then (Arg.usage speclist usage; exit 2);
  
  
  if !lex_sort then sort_lexicon !lexicon
  else if !missing then missing_translation !repo !lexicon !lang
  else if !orphans then missing_or_unused_msg !lexicon !repo !log
in
Printexc.print main () ;;
