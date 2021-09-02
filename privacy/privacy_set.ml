let open Def in
let open Gwdb in

let may_patch base access p =
  if get_access p <> access
  then patch_person base (get_iper p) { (gen_person_of_person p) with access }
in

let access_everybody access base =
  Gwdb.Collection.iter (may_patch base access) (Gwdb.persons base)
in

let access_some access base key =
  match
    match Gutil.person_ht_find_all base key with
    | [ip] -> Some ip
    | _ -> Gutil.person_of_string_dot_key base key
  with
  | Some ip -> may_patch base access (poi base ip)
  | None -> Printf.eprintf "Bad key %s\n" key ; flush stderr
in

let main () =
  let bname = ref "" in
  let access = ref None in
  let ind = ref [] in
  let everybody = ref false in
  let aux v () = assert (!access = None) ; access := Some v in
  let speclist =
    [ ( "-everybody"
      , Arg.Set everybody
      , " Process the whole database."
      )
    ; ( "-ind"
      , Arg.String (fun x -> ind := x :: !ind)
      , "<KEY> Process this individual and its ancestors."
      )
    ; ( "-list-ind"
      , Arg.String begin fun s ->
          let ic = open_in s in
          try while true do ind := (input_line ic) :: !ind done
          with End_of_file -> ()
        end
      , "<FILE> Process the list of persons contained in <FILE> (one key per line)."
      )
    ; ( "-public", Arg.Unit (aux Public), " Set individuals access to Public." )
    ; ( "-private", Arg.Unit (aux Private), " Set individuals access to Private." )
    ; ( "-iftitle", Arg.Unit (aux IfTitles), " Set individuals access to IfTitle." )
    ]
  in
  let anonfun i = bname := i in
  let usage =
    "Usage: cat privacy_set.ml | [GWREPL OPTS] " ^ Sys.argv.(0) ^ " [OPTS] /path/to/base.gwb"
  in
  Arg.parse speclist anonfun usage ;
  let usage () = Arg.usage speclist usage ; exit 2 in
  if !bname = "" then usage () ;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Mutil.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  let base = Gwdb.open_base !bname in
  begin match !access, !ind, !everybody with
    | None, _, _ ->
      prerr_endline "missing -public, -private or -iftitle option" ;
      usage ()
    | _, [], false ->
      prerr_endline "missing -everybody, -ind or -list-ind option" ;
      usage ()
    | _, _ :: _, true ->
      prerr_endline "missing -everybody and -ind/-list-ind options are mutually exclusive" ;
      usage ()
    | Some access, [], true ->
      access_everybody access base
    | Some access, ind, false ->
      List.iter (access_some access base) ind
  end ;
  Gwdb.commit_patches base
in
main ()
;;
