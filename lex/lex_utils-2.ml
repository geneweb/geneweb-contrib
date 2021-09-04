
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
        loop ((lang, transl) :: accu)
      with Not_found -> accu
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

(* Récupère tous les identifiants de message de lexicon. *)
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
      let stop =
        let rec loop k =
          let stop = String.index_from s k '"' in
          if s.[stop - 1] = '\\' then loop (stop + 1)
          else stop
        in
        loop (start + 1)
      in
      list := String.sub s (start + 1) (stop - start - 1) :: !list;
      i := stop + 1
    done;
    !list
  with Not_found -> !list
in

let get_msg_src repl =
  let msg = ref [] in
  let regexp = Str.regexp "transl .* \"" in
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

(* Essaie de chercher tous les identifiants de message du répository et *)
(* recherche s'il ne sont plus utilisés pour au contraire non trdauit.  *)
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
  let plugins = Array.to_list (Sys.readdir repo_plugins) in
  let _ = Printf.printf "Read plugin repo: %d\n" (List.length plugins) in
  let lex =
    if !only then lex
    else 
      let rec loop acc rep =
        match rep with
        | [] -> acc
        | rep :: repl ->
          let rep = Filename.concat rep (Filename.concat "assets" "lex") in
          let rep = Filename.concat repo (Filename.concat "plugins" rep) in
          let _ = Printf.printf "Plugin dir: %s\n" rep in
          if Sys.file_exists rep && Sys.is_directory rep then
            let ll = Array.to_list (Sys.readdir rep) in
            let rec loop2 acc ll =
              begin match ll with
              | [] -> loop acc repl
              | lf :: ll ->
                let _ = Printf.printf "Plugin lex file: %s\n" lf in
                loop2 (List.append acc (get_lexicon_msg lf)) ll
              end
            in loop2 acc ll
          else loop acc repl
      in loop lex plugins
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
    List.iter
      (fun w ->
        if List.mem w msg then ()
        else print_endline w)
      lex;

    Printf.fprintf stdout
      "\nMessage from sources %s (lib, bin, hd/etc, plugins) not in lexicon:\n" repo;
    flush stdout;
    List.iter
      (fun w ->
        if List.mem w lex then ()
        else print_endline w)
      msg
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

let missing_translation lexicon languages =
  match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          let list = get_all_versions ic in
          let list' = missing_languages list languages in
          if list' <> [] then
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
          let list = get_all_versions ic in
          let list' = List.sort (fun (x, _) (y, _) -> compare x y) list in
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
  ; ("-log", Arg.Set log, " Option for orphans. Print in log files instead of stdout.")
  ; ("-sort", Arg.Set lex_sort, " Sort the lexicon (both key and content).")
  ; ("-first", Arg.Set first, " If multiple language entries, select first occurence.")
  ; ("-merge", Arg.Set merge, " Merge rather than replace new lexicon entries.")
  ; ("-only", Arg.Set only, " Process only designated lexicon.")
  ] |> Arg.align
in

let anonfun s = lexicon := if !repo = "" then s else (Filename.concat !repo s) in

let usage = "Usage: cat lex_utils.ml | " ^ Sys.argv.(0) ^ " [options] lexicon" in

let main () =
  Arg.parse speclist anonfun usage;
  if !lexicon = "" then (Arg.usage speclist usage; exit 2);
  if !orphans && !repo = "" then (Arg.usage speclist usage; exit 2);
  if !lex_sort then sort_lexicon !lexicon
  else if !missing then missing_translation !lexicon !lang
  else if !orphans then missing_or_unused_msg !lexicon !repo !log
in
Printexc.print main () ;;
