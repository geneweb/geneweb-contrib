open Gwdb

(** [oldest_year_of p]
    Find a year in [[ birth ; baptism ; death ]].
*)
let oldest_year_of p =
  let open Date in
  match od_of_cdate (get_birth p) with
  | Some (Dgreg (d, _)) -> Some d.year
  | _ -> match od_of_cdate (get_baptism p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match date_of_death (get_death p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> None

(** [most_recent_year_of p]
    Find a year in [[ death ; baptism ; birth ]].
*)
let most_recent_year_of p =
  let open Date in
  match date_of_death (get_death p) with
  | Some (Dgreg (d, _)) -> Some d.year
  | _ -> match od_of_cdate (get_baptism p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match od_of_cdate (get_birth p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> None

let access_everybody access bname =
  let base = Gwdb.open_base bname in
  let n = nb_of_persons base in
  Gwdb.load_persons_array base;
  Gwdb.load_couples_array base;
  ProgrBar.start ();
  Gwdb.Collection.iteri begin fun i p ->
    if get_access p <> access then begin
      let p = {(gen_person_of_person p) with Def.access = access} in
      patch_person base p.Def.key_index p
    end;
    ProgrBar.run i n
  end (Gwdb.persons base) ;
  ProgrBar.finish ();
  Gwdb.clear_persons_array base;
  Gwdb.clear_couples_array base;
  commit_patches base

let change_only_old_access ~old_access ~new_access bname =
  let base = Gwdb.open_base bname in
  Gwdb.Collection.iter begin fun p ->
    if Gwdb.get_access p = old_access then
      let p = {(gen_person_of_person p) with Def.access = new_access} in
      patch_person base p.Def.key_index p
  end (Gwdb.persons base) ;
  commit_patches base
