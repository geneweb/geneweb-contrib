open Gwdb

let access_some_aux base access key =
  match Gutil.person_ht_find_all base key with
  | [ip] ->
    let p = poi base ip in
    if get_access p <> access then begin
      let p = {(gen_person_of_person p) with Def.access = access} in
      patch_person base p.Def.key_index p
    end ;
  | _ ->
    match Gutil.person_of_string_dot_key base key with
    | Some ip ->
      let p = poi base ip in
      if get_access p <> access then begin
        let p = {(gen_person_of_person p) with Def.access = access} in
        patch_person base p.Def.key_index p
      end ;
    | None ->
      Printf.eprintf "Bad key %s\n" key; flush stderr

let access_some access bname list =
  let base = Gwdb.open_base bname in
  List.iter (access_some_aux base access) list ;
  commit_patches base

let input_file list file =
  let ic = open_in file in
  begin
    try while true do list := input_line ic :: !list done
    with End_of_file -> ()
  end ;
  close_in ic

let () =
  let access = ref None in
  let list = ref [] in
  let bname = ref "" in
  let everybody = ref false in
  let set_access x = assert (!access = None) ; access := Some x in
  let access_opt opt doc x =
    (opt, Arg.Unit (fun () -> set_access x), " Set access flag to " ^ doc ^ ".")
  in
  let speclist =
    [ access_opt "--private" "PRIVATE" Def.Private
    ; access_opt "--public" "PUBLIC" Def.Public
    ; access_opt "--default" "IFTITLE" Def.IfTitles
    ; "--everybody", Arg.Set everybody, " process the wole base."
    ; "--key", Arg.String (fun s -> list := s :: !list), "<KEY>"
    ; "--keys", Arg.String (fun s -> input_file list s), "<FILE> file containing one key per line."
    ]
    |> Arg.align
  in
  let anonfun i = bname := i in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [-everybody] [-ind key] [-list-ind file] base" in
  Arg.parse speclist anonfun usage ;
  match !bname, !access, !everybody, !list with
  | ("", _, _, _) | (_, None, _, _) | (_, _, false, []) ->
    Arg.usage speclist usage ;
    exit 2 ;
  | (bname, Some access, everybody, list) ->
    Secure.set_base_dir (Filename.dirname bname);
    Lock.control_retry (Files.lock_file bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
    if everybody then Gwaccess_util.access_everybody access bname
    else access_some access bname list
