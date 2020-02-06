open Geneweb
open Def
open Gwdb

let suspend_with msg = ProgrBar.suspend (); msg () ; flush stdout
let restart_with_fixed i n = Printf.printf "\t\tfixed\n"; flush stdout; ProgrBar.restart i n
let string_of_p base i = Gutil.designation base (poi base i)

let check_burial base ~verbosity1 ~verbosity2 nb_ind cnt =
  if verbosity1 then begin
    Printf.printf "Check persons' burial\n";
    flush stdout;
    ProgrBar.start ()
  end;
  let fix_pevents p =
    let evt =
      { epers_name = Epers_Burial
      ; epers_date = Adef.cdate_None
      ; epers_place = get_burial_place p
      ; epers_reason = empty_string
      ; epers_note = get_burial_note p
      ; epers_src = get_burial_src p
      ; epers_witnesses = [| |]
      }
    in
    patch_person base (get_iper p)
      { (gen_person_of_person p) with pevents = get_pevents p @ [ evt ] }
  in
  let fix_burial p =
    patch_person base (get_iper p)
      { (gen_person_of_person p) with burial = Buried Adef.cdate_None } ;
  in
  let fix p =
    match get_burial p with
    | UnknownBurial ->
      begin
        match
          List.find_opt (fun e -> e.epers_name = Epers_Burial) (get_pevents p)
        with
        | Some _e -> fix_burial p ; true
        | None ->
          if not (is_empty_string (get_burial_place p))
          || not (is_empty_string (get_burial_src p))
          then begin
             fix_burial p ;
             fix_pevents p ;
             true
           end else false
      end
    | _ -> false
  in
  Gwdb.Collection.iteri begin fun i p ->
    if verbosity1 then ProgrBar.run i nb_ind;
    if fix p then begin
      incr cnt ;
      if verbosity2
      then Printf.printf "Modifiy person : %s\n%!" (Gutil.designation base p)
    end
  end (Gwdb.persons base) ;
  if verbosity1 then ProgrBar.finish ()

let check_families_parents ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check families' parents\n";
    flush stdout;
    ProgrBar.start () ;
  end ;
  Gwdb.Collection.iteri begin fun i fam ->
    let ifam = get_ifam fam in
    if verbosity1 then ProgrBar.run i nb_fam ;
    Array.iter begin fun ip ->
      let unions = get_family (poi base ip) in
      if not @@ Array.mem ifam unions then begin
        if verbosity2 then suspend_with (fun () -> Printf.printf "\tNo family for: %s\n" (string_of_p base ip) ) ;
        patch_union base ip { family = Array.append unions [|ifam|] } ;
        incr fix ;
        if verbosity2 then restart_with_fixed i nb_fam ;
      end
    end (get_parent_array fam)
  end (Gwdb.families base) ;
  ProgrBar.finish ()

let check_families_children ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Check families' children\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i fam ->
    let ifam = get_ifam fam in
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
    let ip = get_iper p in
    let fam = Gwdb.foi base ifam in
    if get_ifam fam = dummy_ifam then begin
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
    let ip = get_iper p in
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

let check_pevents_witnesses ~verbosity1 ~verbosity2 base nb_ind fix =
  if verbosity1 then begin
    Printf.printf "Check persons' events witnesses\n";
    flush stdout;
    ProgrBar.start ()
  end;
  Gwdb.Collection.iteri begin fun i p ->
    if verbosity1 then ProgrBar.run i nb_ind;
    let ip = get_iper p in
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

let fix_marriage_divorce ~verbosity1 ~verbosity2 base nb_fam fix =
  if verbosity1 then begin
    Printf.printf "Fix families' marriage and divorce\n";
    flush stdout;
    ProgrBar.start () ;
  end ;
  Gwdb.Collection.iteri begin fun i fam ->
    if verbosity1 then ProgrBar.run i nb_fam;
    let fevents = get_fevents fam in
    let relation0 = get_relation fam in
    let marriage0 = get_marriage fam in
    let marriage_place0 = get_marriage_place fam in
    let marriage_note0 = get_marriage_note fam in
    let marriage_src0 = get_marriage_src fam in
    let divorce0 = get_divorce fam in
    let marr_data0 = (relation0, marriage0, marriage_place0, marriage_note0, marriage_src0) in
    let (relation, marriage, marriage_place, marriage_note, marriage_src) as marr_data, divorce, _ =
      UpdateFamOk.reconstitute_from_fevents false (insert_string base "") fevents
    in
    if marr_data0 <> marr_data || divorce0 <> divorce then begin
      if verbosity2 then begin suspend_with (fun () ->
          Printf.printf "*** Updating: %s & %s\n"
            (Gutil.designation base (poi base @@ get_father fam))
            (Gutil.designation base (poi base @@ get_mother fam)) ) ;
        flush stdout ;
        ProgrBar.restart i nb_fam
      end ;
      let fam' =
        { (gen_family_of_family fam)
          with relation ; marriage ; marriage_place ; marriage_note ; marriage_src ; divorce }
      in
      patch_family base (get_ifam fam) fam' ;
      incr fix
    end ;
  end (Gwdb.families base) ;
  if verbosity1 then ProgrBar.finish ()

let check
    ~verbosity
    ~fast
    ~f_parents
    ~f_children
    ~p_parents
    ~p_families
    ~p_burial
    ~pevents_witnesses
    ~fevents_witnesses
    ~marriage_divorce
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
  if !f_parents then check_families_parents ~verbosity1 ~verbosity2 base nb_fam fix;
  if !f_children then check_families_children ~verbosity1 ~verbosity2 base nb_fam fix;
  if !p_parents then check_persons_parents ~verbosity1 ~verbosity2 base nb_ind fix;
  if !p_burial then check_burial base ~verbosity1 ~verbosity2 nb_ind fix ;
  if !p_families then check_persons_families ~verbosity1 ~verbosity2 base nb_ind fix;
  if !pevents_witnesses then check_pevents_witnesses ~verbosity1 ~verbosity2 base nb_ind fix;
  if !fevents_witnesses then check_fevents_witnesses ~verbosity1 ~verbosity2 base nb_fam fix;
  if !marriage_divorce then fix_marriage_divorce ~verbosity1 ~verbosity2 base nb_fam fix;
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
  Gwdb.sync base ;
  if verbosity1 then (Printf.printf "Done" ; flush stdout)

(**/**)

let bname = ref ""
let verbosity = ref 2
let fast = ref false
let f_parents = ref false
let f_children = ref false
let p_parents = ref false
let p_families = ref false
let p_burial = ref false
let pevents_witnesses = ref false
let fevents_witnesses = ref false
let marriage_divorce = ref false

let speclist =
  [ ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode")
  ; ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode")
  ; ("-fast", Arg.Set fast, " fast mode. Needs more memory.")
  ; ("-families-parents", Arg.Set f_parents, " missing doc")
  ; ("-families-children", Arg.Set f_children, " missing doc")
  ; ("-persons-burial", Arg.Set p_parents, " missing doc")
  ; ("-persons-parents", Arg.Set p_parents, " missing doc")
  ; ("-persons-families", Arg.Set p_families, " missing doc")
  ; ("-pevents-witnesses", Arg.Set pevents_witnesses, " missing doc")
  ; ("-fevents-witnesses", Arg.Set fevents_witnesses, " missing doc")
  ; ("-marriage-divorce", Arg.Set marriage_divorce, " missing doc")
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
  || !pevents_witnesses
  || !fevents_witnesses
  || !marriage_divorce
  || !p_burial
  then ()
  else begin
    f_parents := true ;
    f_children := true ;
    p_parents := true ;
    p_families := true ;
    pevents_witnesses := true ;
    fevents_witnesses := true ;
    marriage_divorce := true ;
    p_burial := true
  end ;
  check
    ~fast
    ~verbosity
    ~f_parents
    ~f_children
    ~p_burial
    ~p_parents
    ~p_families
    ~pevents_witnesses
    ~fevents_witnesses
    ~marriage_divorce
    !bname

let _ = main ()
