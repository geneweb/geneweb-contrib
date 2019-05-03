(* Copyright (c) 2000-2006 INRIA *)
open Def
open Gwdb

let select_ancestors base per_tab fam_tab flag =
  let rec loop iper =
    if Gwdb.Marker.get per_tab iper land flag <> 0 then ()
    else
      begin
        Gwdb.Marker.set per_tab iper (Gwdb.Marker.get per_tab iper lor flag);
        match get_parents (poi base iper) with
        | Some ifam ->
          if Gwdb.Marker.get fam_tab ifam land flag <> 0 then ()
          else
            begin
              Gwdb.Marker.set fam_tab ifam (Gwdb.Marker.get fam_tab ifam lor flag);
              let cpl = foi base ifam in
              loop (get_father cpl); loop (get_mother cpl)
            end
        | None -> ()
      end
  in
  loop

let ok_titles = ["roi"; "reine"; "empereur"]

let has_titles base p =
  List.exists (fun t -> List.mem (sou base t.t_ident) ok_titles)
    (get_titles p)

let parents_has_titles base p =
  match get_parents p with
    Some ifam ->
      let cpl = foi base ifam in
      has_titles base (poi base (get_father cpl)) ||
      has_titles base (poi base (get_mother cpl))
  | None -> false

let gen_good_dates p birth_lim death_lim =
  let death_ok =
    match get_death p with
      Death (_, cd) ->
        begin match Adef.date_of_cdate cd with
          Dgreg (d, _) -> d.year <= death_lim
        | _ -> false
        end
    | _ -> false
  in
  if death_ok then true
  else
    match Adef.od_of_cdate (get_birth p) with
      Some (Dgreg (d, _)) -> d.year <= birth_lim
    | _ -> false

let good_dates base p =
  if gen_good_dates p 1830 1900 then true
  else
    match get_parents p with
      Some ifam ->
        let cpl = foi base ifam in
        gen_good_dates (poi base (get_father cpl)) 1780 1800 ||
        gen_good_dates (poi base (get_mother cpl)) 1780 1800
    | None -> false

let rec select_closure base per_tab fam_tab flag ip =
  if Gwdb.Marker.get per_tab ip = 1 then
    let u = poi base ip in
    Gwdb.Marker.set per_tab ip flag;
    begin match get_parents (poi base ip) with
      | Some ifam ->
        let cpl = foi base ifam in
        select_closure base per_tab fam_tab flag (get_father cpl);
        select_closure base per_tab fam_tab flag (get_mother cpl)
      | None -> ()
    end;
    for i = 0 to Array.length (get_family u) - 1 do
      let ifam = (get_family u).(i) in
      let desc = foi base ifam in
      if Gwdb.Marker.get fam_tab ifam = 1 then
        begin
          Gwdb.Marker.set fam_tab ifam flag;
          for i = 0 to Array.length (get_children desc) - 1 do
            select_closure base per_tab fam_tab flag (get_children desc).(i)
          done;
          ()
        end
    done;
    ()

let functions base _ _ _ _ _ _ _ _ =
  let per_tab = Gwdb.iper_marker (Gwdb.ipers base) 0 in
  let fam_tab = Gwdb.ifam_marker (Gwdb.ifams base) 0 in
  Gwdb.Collection.iter begin fun p ->
    if has_titles base p || parents_has_titles base p || good_dates base p
    then select_ancestors base per_tab fam_tab 1 (get_iper p)
  end (Gwdb.persons base) ;
  Gwdb.Collection.iter begin fun i ->
    if Gwdb.Marker.get per_tab i == 1 then
      let u = poi base i in
      for i = 0 to Array.length (get_family u) - 1 do
        let ifam = (get_family u).(i) in
        let cpl = foi base ifam in
        Gwdb.Marker.set fam_tab ifam 1;
        Gwdb.Marker.set per_tab (get_father cpl) 1;
        Gwdb.Marker.set per_tab (get_mother cpl) 1
      done
  end (Gwdb.ipers base) ;
  match person_of_key base "juan carlos" "de borbon" 0 with
    Some ip ->
      select_closure base per_tab fam_tab 3 ip;
      (fun i -> Gwdb.Marker.get per_tab i == 3),
      (fun i -> Gwdb.Marker.get fam_tab i == 3)
  | None -> failwith "not found juan carlos.0 de borbon"
