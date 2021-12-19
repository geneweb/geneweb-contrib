let open Def in
let open Gwdb in

(** This script is used to set people old enough privacy to [Public]. *)
(** Set privacy of persons older than X years as [Public]. Set the
    ancestors and descendants with no dates [Public] as well (counting 3
    generations by century for descendants).
*)
(** You can also use this script to manually set access of a person list
    using keys.
*)

let nb_gen_by_century = 3 in

let trace = ref false in

let if_trace base p acc =
  if !trace then
    Printf.printf "%s -> %s\n%!"
      (Gutil.designation base p)
      (if acc = Private then "private" else "public")
in

(** Compute the number of (descending) generation to be considered as old
    starting from [p] included. i.e. [0] means that [p] is not considered old.
*)
let compute_ndgen treshold y =
  (treshold - y) * nb_gen_by_century / 100
in

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
      let ndgen = match most_recent_year_of p with
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
                   Opt.map_default ndgen
                     (compute_ndgen treshold)
                     (most_recent_year_of (poi base sp))
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
in

let mark_ancestors base scanned treshold =
  let rec loop p =
    let i = get_iper p in
    if not @@ Gwdb.Marker.get scanned i then begin
      Gwdb.Marker.set scanned i true ;
      begin match oldest_year_of p with
        | Some y when y >= treshold ->
          Printf.eprintf "Problem of date ! %s %d\n" (Gutil.designation base p) y;
          flush stderr
        | _ ->
          if get_access p <> Public
          && not (is_quest_string (get_first_name p))
          && not (is_quest_string (get_surname p))
          then begin
            if_trace base p Public ;
            let p = {(gen_person_of_person p) with access = Public} in
            patch_person base p.key_index p ;
          end
      end ;
      Opt.iter
        (fun ifam ->
           let cpl = foi base ifam in
           loop (poi base (get_father cpl)) ;
           loop (poi base (get_mother cpl)) )
        (get_parents p)
    end
  in
  loop
in

let public_all ~mem base treshold =
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
  end
in

let main () =
  let default_treshold = 1900 in
  let treshold = ref default_treshold in
  let bname = ref "" in
  let mem = ref false in
  let speclist =
    [ ( "-y"
      , Arg.Set_int treshold
      , "<YEAR> Treshold year. Anybody born before this year is considered old (default = "
        ^ string_of_int default_treshold ^ ")"
      )
    ; ( "-mem", Arg.Set mem, " Save memory (slower)." )
    ; ( "-trace", Arg.Set trace, " Trace changes." )
    ] |> Arg.align
  in
  let anonfun i = bname := i in
  let usage =
    "Usage: cat privacy_auto.ml | [GWREPL OPTS] " ^ Sys.argv.(0) ^ " [OPTS] /path/to/base.gwb"
  in
  Arg.parse speclist anonfun usage ;
  let usage () = Arg.usage speclist usage ; exit 2 in
  if !bname = "" then usage () ;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Mutil.lock_file !bname) ~onerror:Lock.print_error_and_exit begin fun () ->
    public_all ~mem:!mem (Gwdb.open_base !bname) (!threshold)
      Gwdb.commit_patches base
  end
in

main ()
;;
