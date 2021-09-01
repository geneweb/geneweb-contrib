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

let get_msg_src repo =
  let msg = ref [] in
  (* TODO the current setup misses translations with the string on the next line !! *)
  let regexp = Str.regexp "transl.* \"" in
  let _ = "\"" in (* just for quotes balancing in BBedit. Putting it in comment fails!! *)
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
    (get_ml_files repo);
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

let get_msg_tpl repo =
  let msg = ref [] in
  let regexp = Str.regexp "[*?[a-z]+]" in
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
    (get_tpl_files repo);
  List.fold_left
    (fun accu msg -> List.rev_append (cut_all_msg msg) accu)
    [] !msg
in

let module StringSet = Set.Make
    (struct
      type t = string
      let compare = Stdlib.compare
    end)
in

let sort_uniq cmp l =
  let list =
    List.fold_left
      (fun accu e -> StringSet.add e accu)
      StringSet.empty l
  in
  List.sort cmp (StringSet.elements list)
in

(* Essaie de chercher tous les identifiants de message du répository et *)
(* recherche s'il ne sont plus utilisés pour au contraire non traduit.  *)
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
  let repo_src = Filename.concat repo "lib" in
  let repo_tpl =
    List.fold_left Filename.concat repo ["hd"; "etc"]
  in

  let lex = get_lexicon_msg lexicon in
  let msg_src = get_msg_src repo in
  let msg_tpl = get_msg_tpl repo_tpl in
  let msg =
    sort_uniq
      (fun x y ->
         Stdlib.compare
           (String.lowercase_ascii x) (String.lowercase_ascii y))
      (List.rev_append msg_src msg_tpl)
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
      "\nMessage not used anymore in %s and %s :\n" repo_src repo_tpl;
    flush stdout;
    List.iter
      (fun w ->
         if List.mem w msg then ()
         else print_endline w)
      lex;
    Printf.fprintf stdout
      "\nMessage from %s and %s not in lexicon :\n" repo_src repo_tpl;
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


(**/**) (* Main. *)

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
let repo = ref "" in
let log = ref false in

let speclist =
  [ ("-first", Arg.Set first, " If multiple language entries, select first occurence.")
  ; ("-log", Arg.Set log, " Option for repo. Print in log files instead of stdout.")
  ; ("-merge", Arg.Set merge, " Merge rather than replace new lexicon entries.")
  ; ("-missing", Arg.Set missing
    , " Print missing translation for these lang: " ^ String.concat "," lang_default ^".")
  ; ( "-missing-lang", Arg.String (fun s -> missing := true ; lang := String.split_on_char ',' s)
    , " Same as -missing, but use a comma-separated list of lang instead of the default one.")
  ; ("-repo", Arg.String (fun x -> repo := x), " Check missing or unused keyword.")
  ; ("-sort", Arg.Set lex_sort, " Sort the lexicon (both key and content).")
  ] |> Arg.align
in

let anonfun s = lexicon := s in
let usage = "Usage: cat lex_utils.ml | " ^ Sys.argv.(0) ^ " [options] lexicon" in
let main () =
  Arg.parse speclist anonfun usage;
  if !lexicon = "" then (Arg.usage speclist usage; exit 2);
  if !lex_sort then sort_lexicon !lexicon
  else if !missing then missing_translation !lexicon !lang
  else if !repo <> "" then missing_or_unused_msg !lexicon !repo !log
in

Printexc.print main () ;;
