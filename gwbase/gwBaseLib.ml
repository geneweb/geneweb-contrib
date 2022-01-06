(* $Id: gwlib.ml,v 4.21 2007-01-18 19:44:50 deraugla Exp $ *)

open Def
open Gwdb

let add_indi base (fn, sn, nb) sex =
  let empty = empty_person base dummy_iper in
  let np = { (gen_person_of_person empty) with
             first_name = insert_string base fn
           ; surname = insert_string base sn
           ; occ = nb
           ; sex
           }
  in
  let na = gen_ascend_of_person empty in
  let nu = gen_union_of_person empty in
  insert_person base np na nu

let add_fam base fath moth children =
  let fam = gen_family_of_family (empty_family base dummy_ifam) in
  let cpl = Adef.couple fath moth in
  let des = { children = Array.of_list children } in
  let ufath = poi base fath in
  let umoth = poi base moth in
  let ifam = insert_family base fam cpl des in
  patch_couple base ifam cpl;
  patch_descend base ifam des;
  let ufath = { family = Array.append (get_family ufath) [| ifam |] } in
  patch_union base fath ufath;
  let umoth = { family = Array.append (get_family umoth) [| ifam |] } in
  patch_union base moth umoth;
  List.iter (fun ip ->
      let a = { parents = Some ifam; consang = Adef.fix (-1) } in
      patch_ascend base ip a
    ) children;
  ifam
