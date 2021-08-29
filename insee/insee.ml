(* Copyright (c) 2019 Ludovic LEDIEU *)
(* Adjusted for gwrepl by Henri *)
(*
  use: cat <script.ml> | [ GWREPL_VERBOSE=1 ] [ GWREPL_FORCE_UNPACK=1 ]
   [ GWREPL_NOPROMPT=1 ] gwrepl.exe [script_arg1] ...

*)

let () =

let open Def in
let open Gwdb in
let open Arg in

let my_uppercase2 s =
  let s = String.split_on_char '\'' s in
  String.concat
    (String.make 1 '\'')
    (List.map (fun e -> String.uppercase_ascii (Name.lower e)) s)
in

let my_uppercase s =
  let s = String.split_on_char '-' s in
  String.concat
    (String.make 1 '-')
    (List.map (fun e -> my_uppercase2 e) s)
in

let date_of_death = (* from Date *)
  function
    Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | _ -> None
in

let check_insee base =
  (* pour chaque personne *)
  Gwdb.Collection.iteri begin fun i p ->
    let b_date =
      match Adef.od_of_cdate (get_birth p) with
        Some (Dgreg (d, _)) -> Some d
      | _ -> None
    in
    let d_date =
      match date_of_death (get_death p) with
        Some (Dgreg (d, _)) -> Some d
      | _ -> None
    in
    let sn = my_uppercase (p_surname base p) in
    let fn =
      let fn = p_first_name base p in
      begin match get_first_names_aliases p with
        [] -> fn
      | first :: _ ->
          let fna = sou base first in
          let re = Str.regexp_string (Str.quote fn) in
          begin try ignore (Str.search_forward re fna 0); fna
          with Not_found -> fn
          end
      end
    in
    let fn = my_uppercase fn in
    let s =
      match get_sex p with
        Male -> 1
      | Female -> 2
      | _ -> 0
    in
    let b_place = sou base (get_birth_place p) in
    let d_place = sou base (get_death_place p) in
    let key =
      sou base (get_first_name p) ^ "." ^ string_of_int (get_occ p) ^ " " ^ sou base (get_surname p)
    in
    let check =
      fn != "X" &&
      (b_place = "" || (Mutil.contains b_place ", France") ||
       d_place = "" || (Mutil.contains d_place ", France")) &&
      match d_date with
      | Some dd when dd.prec = Sure && dd.year < 1970 -> false
      | Some dd when dd.prec = Before && dd.year < 1970 -> false
      | _ -> true
    in
    match b_date with
      Some bd when bd.year > 1870 && check = true ->
        begin match d_date with
        | Some dd -> 
            Printf.printf "%s|%s|%d|%02d|%02d|%04d|%s|%02d|%02d|%04d|%s|%s\n" sn fn s
              (if bd.prec = Sure then bd.day else 0)
              (if bd.prec = Sure then bd.month else 0)
              (if bd.prec = Sure || bd.prec = About then bd.year else 0)
              b_place
              (if dd.prec = Sure then dd.day else 0)
              (if dd.prec = Sure then dd.month else 0)
              (if dd.prec = Sure || dd.prec = About then dd.year else 0)
              d_place
              key
        | None ->
            Printf.printf "%s|%s|%d|%02d|%02d|%04d|%s|00|00|0000|%s|%s\n" sn fn s
              (if bd.prec = Sure then bd.day else 0)
              (if bd.prec = Sure then bd.month else 0)
              (if bd.prec = Sure || bd.prec = About then bd.year else 0)
              b_place
              d_place
              key
        end
    | _ ->
      if check then
      begin match d_date with
      | Some dd when dd.year > 1970 ->
            Printf.printf "%s|%s|%d|00|00|0000|%s|%02d|%02d|%04d|%s|%s\n" sn fn s
              b_place
              (if dd.prec = Sure then dd.day else 0)
              (if dd.prec = Sure then dd.month else 0)
              (if dd.prec = Sure || dd.prec = About then dd.year else 0)
              d_place
              key
      | _ -> ()
      end
  end (Gwdb.persons base)
in

(* main *)

let fname = ref "" in

let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <database>" in

let no_lock_flag = ref false in (* from Lock *)

let speclist =
  [("-nolock", Arg.Set no_lock_flag, ": do not lock data base")]
in

let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several data bases")
  fname :=
    if Filename.extension !fname = ".gwb" then !fname
    else !fname ^ ".gwb"
in

let main () =
  Arg.parse speclist anonfun errmsg;
  if !fname = "" then begin
    Printf.eprintf "Missing file name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2
  end
  else
    let base = open_base !fname in
    let () = load_strings_array base in
    check_insee base
in

Printexc.catch main ();;
