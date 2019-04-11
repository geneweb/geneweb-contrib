open Geneweb
open Gwdb

(** [oldest_year_of p]
    Find a year in [[ birth ; baptism ; death ]].
*)
let oldest_year_of p =
  let open Def in
  match Adef.od_of_cdate (get_birth p) with
  | Some (Dgreg (d, _)) -> Some d.year
  | _ -> match Adef.od_of_cdate (get_baptism p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match CheckItem.date_of_death (get_death p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> None

(** [most_recent_year_of p]
    Find a year in [[ death ; baptism ; birth ]].
*)
let most_recent_year_of p =
  let open Def in
  match CheckItem.date_of_death (get_death p) with
  | Some (Dgreg (d, _)) -> Some d.year
  | _ -> match Adef.od_of_cdate (get_baptism p) with
    | Some (Dgreg (d, _)) -> Some d.year
    | _ -> match Adef.od_of_cdate (get_birth p) with
      | Some (Dgreg (d, _)) -> Some d.year
      | _ -> None

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
  loop 1 [get_key_index p]

let input_person file =
  let pl = ref [] in
  begin match (try Some (open_in file) with Sys_error _ -> None) with
    Some ic ->
      begin try
        while true do let line = input_line ic in pl := line :: !pl done
      with End_of_file -> ()
      end;
      close_in ic
  | None -> Printf.eprintf "Error while opening file %s\n" file; flush stderr
  end;
  List.rev !pl

let access_everybody access bname =
  let base = Gwdb.open_base bname in
  Gwdb.Collection.iter begin fun p ->
    if get_access p <> access then
      let p = {(gen_person_of_person p) with Def.access = access} in
      patch_person base p.Def.key_index p
  end (Gwdb.persons base) ;
  commit_patches base

let access_some access bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      if get_access p <> access then
        begin let p = {(gen_person_of_person p) with Def.access = access} in
          patch_person base p.Def.key_index p
        end;
      commit_patches base
  | _ ->
      match Gutil.person_of_string_dot_key base key with
        Some ip ->
          let p = poi base ip in
          if get_access p <> access then
            begin let p =
              {(gen_person_of_person p) with Def.access = access}
            in
              patch_person base p.Def.key_index p
            end;
          commit_patches base
      | None -> Printf.eprintf "Bad key %s\n" key; flush stderr

let access_some_list access bname file =
  if Sys.file_exists file then
    let pl = input_person file in List.iter (access_some access bname) pl
  else
    begin
      Printf.eprintf "File does not exist : %s\n" file;
      flush stderr;
      exit 2
    end
