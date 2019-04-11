open Geneweb
open Def
open Gwdb

let suspend_with msg = ProgrBar.suspend (); msg () ; flush stdout
let restart_with_fixed i n = Printf.printf "\t\tfixed\n"; flush stdout; ProgrBar.restart i n
let string_of_p base i = Gutil.designation base (poi base i)

let check_families_parents ~verbosity1 ~verbosity2 base nb_fam =
  if verbosity1 then begin
    Printf.printf "Check families' parents\n";
    flush stdout;
    ProgrBar.start () ;
  end ;
  Gwdb.Collection.iteri begin fun i fam ->
    let ifam = get_fam_index fam in
    if verbosity1 then ProgrBar.run i nb_fam ;
    let a = get_parent_array fam in
    for j = 0 to Array.length a - 1 do
      let ip = a.(j) in
      if ip = dummy_iper then begin
        if verbosity2 then begin
          suspend_with (fun () ->
              Printf.printf "\tdummy parent in family %s\n" (string_of_ifam ifam) ) ;
          flush stdout ;
          ProgrBar.restart i nb_fam
        end ;
      end else if not @@ Array.mem ifam (get_family (poi base ip)) then
        begin
          suspend_with (fun () ->
              Printf.printf "\tno family for : %s\n" (string_of_p base ip) ) ;
          flush stdout ;
          ProgrBar.restart i nb_fam
        end
    done
  end (Gwdb.families base) ;
  ProgrBar.finish ()

let check_families_children ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check families' children\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i fam ->
    let ifam = get_fam_index fam in
    if verbosity1 then ProgrBar.run i nb_fam;
    let children = get_children fam in
    for j = 0 to Array.length children - 1 do
      let ip = children.(j) in
      let a = poi base ip in
      let parents = get_parents a in
      if parents = Some dummy_ifam || parents = None then begin
        if verbosity2 then begin
          ProgrBar.suspend ();
          Printf.printf "\tno parents: %s in family [%s & %s]\n"
            (string_of_p base ip)
            (string_of_p base @@ get_father fam)
            (string_of_p base @@ get_mother fam);
          flush stdout
        end ;
        patch_ascend base ip {parents = Some ifam; consang = get_consang a};
        incr fix ;
        if verbosity1 then ProgrBar.restart i nb_fam ;
      end
      else if parents <> Some ifam && verbosity1 then begin
        (* FIXME: what to do here ? *)
        Printf.printf "\tbad parents : %s\n" (string_of_p base ip);
        flush stdout
      end
    done
  end (Gwdb.families base) ;
  if verbosity1 then ProgrBar.finish ()

let check_persons_parents ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' parents\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i p ->
    if verbosity1 then ProgrBar.run i nb_ind;
    get_parents p |> Opt.iter @@ fun ifam ->
    let ip = get_key_index p in
    let fam = Gwdb.foi base ifam in
    if get_fam_index fam = dummy_ifam then begin
      if verbosity2 then begin
        Printf.printf "\tparent family deleted: %s\n" (string_of_p base ip) ;
        flush stdout
      end ;
      patch_ascend base ip {parents = None; consang = Adef.fix (-1)};
      incr fix
    end
    else
      let children = get_children fam in
      if not @@ Array.mem ip children then
        begin
          if verbosity2 then begin
            Printf.printf "\tnot in parent's family: %s\n" (string_of_p base ip);
            flush stdout
          end ;
          let children = Array.append children [| ip |] in
          patch_descend base ifam {children = children}; incr fix
        end
  end (Gwdb.persons base) ;
  if verbosity1 then ProgrBar.finish ()

let check_persons_families ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' families\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i p ->
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = get_key_index p in
    let ifams = get_family p in
    let ifams' =
      Array.of_list @@
      Array.fold_right
        (fun ifam acc ->
           let cpl = foi base ifam in
           if not @@ Array.mem ip (get_parent_array cpl) then
             begin
               if verbosity2 then
                 suspend_with (fun () ->
                     Printf.printf "\tRemoving ifam %s from [%s] unions\n"
                       (string_of_ifam ifam)
                       (string_of_p base ip));
               incr fix ;
               acc
             end
           else ifam :: acc)
        ifams []
    in
    if ifams' <> ifams then patch_union base ip {family = ifams'} ;
  end (Gwdb.persons base) ;
  if verbosity1 then ProgrBar.finish ()

(* FIXME? let imoth = get_mother fam in *)
let check_witnesses ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i fam ->
    if verbosity1 then ProgrBar.run i nb_fam;
    let witn = get_witnesses fam in
    let ifath = get_father fam in
    for j = 0 to Array.length witn - 1 do
      let ip = witn.(j) in
      let p = poi base ip in
      if not (List.memq ifath (get_related p)) then
        begin
          if verbosity2 then
            suspend_with (fun () ->
                let imoth = get_mother fam in
                Printf.printf "\tin marriage: %s & %s\n"
                  (string_of_p base ifath)
                  (string_of_p base imoth);
                Printf.printf "\twitness has no pointer to marriage: %s\n"
                  (string_of_p base ip) ) ;
          patch_person base ip
            {(gen_person_of_person p) with related = ifath :: get_related p};
          incr fix;
          if verbosity2 then restart_with_fixed i nb_fam
        end
    done
  end (Gwdb.families base) ;
  if verbosity1 then ProgrBar.finish ()

let check_pevents_witnesses ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' events witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i p ->
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = get_key_index p in
    List.iter
      (fun evt ->
         let witn = Array.map fst evt.epers_witnesses in
         for j = 0 to Array.length witn - 1 do
           let ip2 = witn.(j) in
           let p2 = poi base ip2 in
           if not (List.memq ip (get_related p2)) then
             begin
               if verbosity2 then suspend_with (fun () ->
                   Printf.printf "\tin persons' event: %s\n" (string_of_p base ip2);
                   Printf.printf "\twitness has no pointer to persons' event: %s\n" (string_of_p base ip) ) ;
               patch_person base ip2
                 {(gen_person_of_person p2)
                  with related = ip :: get_related p2};
               incr fix;
               if verbosity2 then restart_with_fixed i nb_ind
             end
         done)
      (get_pevents p)
  end (Gwdb.persons base) ;
  if verbosity1 then ProgrBar.finish ()

let check_fevents_witnesses ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check family events witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i fam ->
    if verbosity1 then ProgrBar.run i nb_fam;
    let ifath = get_father fam in
    List.iter
      (fun evt ->
         let witn = Array.map fst evt.efam_witnesses in
         for j = 0 to Array.length witn - 1 do
           let ip = witn.(j) in
           let p = poi base ip in
           if not (List.memq ifath (get_related p)) then
             begin
               if verbosity2 then
                 suspend_with (fun () ->
                     let imoth = get_mother fam in
                     Printf.printf "\tin family event: [%s & %s]\n"
                       (string_of_p base ifath) (string_of_p base imoth);
                     Printf.printf "\twitness has no pointer to family event: %s\n"
                       (string_of_p base  ip) ) ;
               patch_person base ip
                 {(gen_person_of_person p)
                  with related = ifath :: get_related p};
               incr fix;
               if verbosity2 then restart_with_fixed i nb_fam
             end
         done)
      (get_fevents fam)
  end (Gwdb.families base) ;
  if verbosity1 then ProgrBar.finish ()

let check
    ~verbosity
    ~fast
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~witnesses
    ~pevents_witnesses
    ~fevents_witnesses
    bname =
  let verbosity1 = !verbosity >= 1 in
  let verbosity2 = !verbosity >= 2 in
  if not verbosity1 then Mutil.verbose := false ;
  let fast = !fast in
  let base = Gwdb.open_base bname in
  let fix = ref 0 in
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  if fast then begin load_strings_array base ; load_persons_array base end ;
  if !f_parents then check_families_parents ~verbosity1 ~verbosity2 base nb_fam;
  if !f_children then check_families_children ~verbosity1 ~verbosity2 base nb_fam fix;
  if !p_parents then check_persons_parents ~verbosity1 ~verbosity2 base nb_ind fix;
  if !p_families then check_persons_families ~verbosity1 ~verbosity2 base nb_ind fix;
  if !witnesses then check_witnesses ~verbosity1 ~verbosity2 base nb_fam fix;
  if !pevents_witnesses then check_pevents_witnesses ~verbosity1 ~verbosity2 base nb_ind fix;
  if !fevents_witnesses then check_fevents_witnesses ~verbosity1 ~verbosity2 base nb_fam fix;
  if fast then begin clear_strings_array base ; clear_persons_array base end ;
  if !fix <> 0 then begin
    Gwdb.commit_patches base ;
    if verbosity1 then begin
      Printf.printf "%n changes commited\n" !fix ;
      flush stdout
    end
  end
  else if verbosity1 then begin
    Printf.printf "No change\n" ;
    flush stdout
  end ;
  if verbosity1 then (Printf.printf "Rebuilding the indexes..\n" ; flush stdout) ;
  begin match Gwdb.ascends_array base with
  | (_, _, _, None) ->
    Gwdb1.apply_base1 (Gwdb1.OfGwdb.base base)
      (fun base -> Outbase.gen_output false base.Dbdisk.data.Dbdisk.bdir base)
  | _ -> ()
  end ;
  (* On recalcul le nombre reel de personnes. *)
  Util.init_cache_info bname base ;
  if verbosity1 then (Printf.printf "Done" ; flush stdout)

(**/**)

let bname = ref ""
let verbosity = ref 2
let fast = ref false
let f_parents = ref false
let f_children = ref false
let p_parents = ref false
let p_families = ref false
let witnesses = ref false
let pevents_witnesses = ref false
let fevents_witnesses = ref false

let speclist =
  [ ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode")
  ; ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode")
  ; ("-fast", Arg.Set fast, " fast mode. Needs more memory.")
  ; ("-families-parents", Arg.Set f_parents, " missing doc")
  ; ("-families-children", Arg.Set f_children, " missing doc")
  ; ("-persons-parents", Arg.Set p_parents, " missing doc")
  ; ("-persons-families", Arg.Set p_families, " missing doc")
  ; ("-witnesses", Arg.Set witnesses, " missing doc")
  ; ("-pevents-witnesses", Arg.Set pevents_witnesses, " missing doc")
  ; ("-fevents-witnesses", Arg.Set fevents_witnesses, " missing doc")
  ]

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  Secure.set_base_dir (Filename.dirname !bname);
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again @@
  fun () ->
  if !f_parents
  || !f_children
  || !p_parents
  || !p_families
  || !witnesses
  || !pevents_witnesses
  || !fevents_witnesses
  then ()
  else begin
    f_parents := true ;
    f_children := true ;
    p_parents := true ;
    p_families := true ;
    witnesses := true ;
    pevents_witnesses := true ;
    fevents_witnesses := true
  end ;
  check
    ~fast
    ~verbosity
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~witnesses
    ~pevents_witnesses
    ~fevents_witnesses
    !bname

let _ = main ()
