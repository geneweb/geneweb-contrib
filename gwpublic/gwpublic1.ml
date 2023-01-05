open Def
open Gwdb

(** This script is used to set people old enough privacy to [Public]. *)
(** Set privacy of persons older than X years as [Public]. Set the
    ancestors and descendants with no dates [Public] as well (counting 3
    generations by century for descendants).
*)

let nb_gen_by_century = 3

let changes = ref false

(** Compute the number of (descending) generation to be considered as old
    starting from [p] included. i.e. [0] means that [p] is not considered old.
*)
let compute_ndgen treshold y =
  (treshold - y) * nb_gen_by_century / 100

(** Recursively mark descendants and spouses as old,
    as long as a date allow you to do so, or until
    the number of generations that should be considered old according
    to latest known date is reached.
*)
let mark_descendants base scanned old treshold =
  let rec loop p ndgen =
    let p_key_index = get_iper p in
    if Gwdb.Marker.get scanned p_key_index < ndgen then begin
      (* If we did not already scanned with ndgen >= current ndgen *)
      let ndgen = match Gwaccess_util.most_recent_year_of p with
        | Some y ->
          (* We have a date: we do not want to scan this person again with a higher ndgen *)
          Gwdb.Marker.set scanned p_key_index max_int ;
          compute_ndgen treshold y
        | None ->
          Gwdb.Marker.set scanned p_key_index ndgen ;
          ndgen
      in
      if ndgen > 0 then
        begin
          let ndgen' = ndgen - 1 in
          Gwdb.Marker.set old p_key_index true ;
          Array.iter
            (fun ifam ->
               let fam = foi base ifam in
               let sp = Gutil.spouse p_key_index fam in
               if Gwdb.Marker.get scanned sp < ndgen then begin
                 let ndgen'' =
                   Option.fold ~none:ndgen
                     ~some:(compute_ndgen treshold)
                     (Gwaccess_util.most_recent_year_of (poi base sp))
                 in
                 if ndgen'' > 0 then begin
                   Gwdb.Marker.set old sp true ;
                   Array.iter
                     (fun c -> loop (poi base c) (min ndgen' (max 0 (ndgen'' - 1))))
                     (get_children fam)
                 end
               end)
            (get_family p)
        end
    end
  in
  loop

let mark_ancestors base scanned treshold =
  let rec loop p =
    let i = get_iper p in
    if not @@ Gwdb.Marker.get scanned i then begin
      Gwdb.Marker.set scanned i true ;
      begin match Gwaccess_util.oldest_year_of p with
        | Some y when y >= treshold ->
          Printf.eprintf "Problem of date ! %s %d\n" (Gutil.designation base p) y;
          flush stderr
        | _ ->
          if get_access p <> Public
          && not (is_quest_string (get_first_name p))
          && not (is_quest_string (get_surname p))
          then begin
            let p = {(gen_person_of_person p) with access = Public} in
            patch_person base p.key_index p ;
            changes := true
          end
      end ;
      Option.iter
        (fun ifam ->
           let cpl = foi base ifam in
           loop (poi base (get_father cpl)) ;
           loop (poi base (get_mother cpl)) )
        (get_parents p)
    end
  in
  loop

let public_all ~mem bname treshold =
  let base = Gwdb.open_base bname in
  if not mem then begin
    load_persons_array base ;
    load_ascends_array base ;
    load_couples_array base ;
  end ;
  Consang.check_noloop base
    (function
        OwnAncestor p ->
        Printf.printf "I cannot deal this database.\n";
        Printf.printf "%s is his own ancestors\n" (Gutil.designation base p);
        flush stdout;
        exit 2
      | _ -> assert false);
  let nb = nb_of_persons base in
  let ipers = Gwdb.ipers base in
  let old = Gwdb.iper_marker ipers false in
  let scanned = Gwdb.iper_marker ipers (-1) in
  ProgrBar.start () ;
  Gwdb.Collection.iteri begin fun i ip ->
    ProgrBar.run i nb ;
    if Gwdb.Marker.get scanned ip < 0 then
      let p = poi base ip in
      mark_descendants base scanned old treshold p 0
  end ipers ;
  ProgrBar.finish () ;
  let ipers = Gwdb.ipers base in
  let scanned = Gwdb.iper_marker ipers false in
  ProgrBar.start () ;
  Gwdb.Collection.iteri begin fun i ip ->
    ProgrBar.run i nb ;
    if Gwdb.Marker.get old ip && not @@ Gwdb.Marker.get scanned ip then
      let p = poi base ip in
      mark_ancestors base scanned treshold p
  end ipers ;
  ProgrBar.finish () ;
  if not mem then begin
    clear_persons_array base ;
    clear_ascends_array base ;
    clear_couples_array base ;
  end ;
  if !changes then commit_patches base

let public_some bname treshold key =
  let base = Gwdb.open_base bname in
  let ip = match Gutil.person_ht_find_all base key with
    | [ip] -> ip
    | _ -> match Gutil.person_of_string_dot_key base key with
      | Some ip -> ip
      | None -> Printf.eprintf "Bad key %s\n" key; flush stderr; exit 2
  in
  let p = poi base ip in
  let scanned = Gwdb.iper_marker (Gwdb.ipers base) false in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  mark_ancestors base scanned treshold p;
  let () = clear_ascends_array base in
  let () = clear_couples_array base in
  if !changes then commit_patches base

let treshold = ref 1900
let ind = ref ""
let bname = ref ""
let everybody = ref false
let mem = ref false

let speclist =
  ["-y", Arg.Int (fun i -> treshold := i),
   "treshold year. Anybody born before this year is considered old (default = " ^ string_of_int !treshold ^ ")";
   "-everybody", Arg.Set everybody, "set flag public to everybody.";
   "-ind", Arg.String (fun x -> ind := x), "individual key. Process only this individual and its ancestors.";
   "-mem", Arg.Set mem, "save memory (slower)";
  ]
let anonfun i = bname := i
let usage = "Usage: public1 [-everybody] [-mem] [-y #] [-ind key] base"

let () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage ; exit 2 end ;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Files.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  if !everybody then
    if !ind <> "" then failwith "-everybody and -ind options are mutually exclusive"
    else if !treshold <> 1900 then failwith "-everybody and -y options are mutually exclusive"
    else Gwaccess_util.access_everybody Def.Public !bname
  else if !ind = "" then public_all ~mem:!mem !bname !treshold
  else public_some !bname !treshold !ind
