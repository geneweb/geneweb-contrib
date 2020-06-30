let open Def in
let open Gwdb in

(** [oldest_year_of p]
    Find a year in [[ birth ; baptism ; death ]].
*)
let oldest_year_of p =
  match Adef.od_of_cdate (get_birth p) with
  | Some (Dgreg (d, _)) -> Some d.year
  | _ -> match Adef.od_of_cdate (get_baptism p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match Date.date_of_death (get_death p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> None
in

(** [most_recent_year_of p]
    Find a year in [[ death ; baptism ; birth ]].
*)
let most_recent_year_of p =
  match Date.date_of_death (get_death p) with
  | Some (Dgreg (d, _)) -> Some d.year
  | _ -> match Adef.od_of_cdate (get_baptism p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match Adef.od_of_cdate (get_birth p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> None
in

let find_dated_ancestor base p =
  let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
  let rec loop nb_gen iplist =
    if iplist = [] then None
    else
      let anc_list =
        List.fold_left
          (fun anc_list ip ->
             match get_parents (poi base ip) with
               Some ifam ->
                 let fam = foi base ifam in
                 get_mother fam :: get_father fam :: anc_list
             | None -> anc_list)
          [] iplist
      in
      (* Dans le cas où le nombre d'implexes est très élevé, le calcul *)
      (* peut être très long car on le refait plusieurs fois pour les  *)
      (* mêmes personnes. On rend donc la liste unique.                *)
      let anc_list = List.sort_uniq compare anc_list in
      let anc_list =
        List.filter (fun ip -> not @@ Gwdb.Marker.get mark ip) anc_list
      in
      let () =
        List.iter (fun ip -> Gwdb.Marker.set mark ip true) anc_list
      in
      let rec loop_ind =
        function
          ip :: iplist ->
            let p = poi base ip in
            begin match oldest_year_of p with
              Some year -> Some (p, year, nb_gen)
            | None -> loop_ind iplist
            end
        | [] -> loop (nb_gen + 1) anc_list
      in
      loop_ind anc_list
  in
  loop 1 [get_iper p]
in

let access_everybody access base =
  Gwdb.Collection.iter begin fun p ->
    if get_access p <> access then
      let p = {(gen_person_of_person p) with Def.access = access} in
      patch_person base p.Def.key_index p
  end (Gwdb.persons base)
in

let access_some access base key =
  match
    match Gutil.person_ht_find_all base key with
    | [ip] -> Some ip
    | _ -> Gutil.person_of_string_dot_key base key
  with
  | Some ip ->
    let p = poi base ip in
    if get_access p <> access then begin
      let p = { (gen_person_of_person p) with Def.access = access } in
      patch_person base p.Def.key_index p
    end
  | None -> Printf.eprintf "Bad key %s\n" key ; flush stderr
in
