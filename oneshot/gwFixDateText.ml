open Def
open Gwdb

let fix_date_text bname =
  let base = Gwdb.open_base bname in
  let space_to_unders = Mutil.tr ' ' '_' in
  Gwdb.Collection.iter
    (fun p ->
      let birth = get_birth p in
      let birth =
        match Date.od_of_cdate birth with
        | Some d -> (
            match d with
            | Dgreg (_, _) -> birth
            | Dtext t ->
                if String.length t < 10 then birth
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Date.cdate_of_od (Some (Dtext (sub_date ^ end_date))))
        | None -> birth
      in
      let baptism = get_baptism p in
      let baptism =
        match Date.od_of_cdate baptism with
        | Some d -> (
            match d with
            | Dgreg (_, _) -> baptism
            | Dtext t ->
                if String.length t < 10 then baptism
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Date.cdate_of_od (Some (Dtext (sub_date ^ end_date))))
        | None -> baptism
      in
      let death = get_death p in
      let death =
        match death with
        | Death (dr, cd) -> (
            let d = Date.date_of_cdate cd in
            match d with
            | Dgreg (_, _) -> death
            | Dtext t ->
                if String.length t < 10 then death
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Death (dr, Date.cdate_of_date (Dtext (sub_date ^ end_date))))
        | _ -> death
      in
      let burial = get_burial p in
      let burial =
        match burial with
        | Buried d -> (
            match Date.od_of_cdate d with
            | Some d -> (
                match d with
                | Dgreg (_, _) -> burial
                | Dtext t ->
                    if String.length t < 10 then burial
                    else
                      let sub_date = String.sub t 0 10 in
                      let end_date = String.sub t 10 (String.length t - 10) in
                      let sub_date = space_to_unders sub_date in
                      Buried
                        (Date.cdate_of_od (Some (Dtext (sub_date ^ end_date)))))
            | None -> burial)
        | Cremated d -> (
            match Date.od_of_cdate d with
            | Some d -> (
                match d with
                | Dgreg (_, _) -> burial
                | Dtext t ->
                    if String.length t < 10 then burial
                    else
                      let sub_date = String.sub t 0 10 in
                      let end_date = String.sub t 10 (String.length t - 10) in
                      let sub_date = space_to_unders sub_date in
                      Cremated
                        (Date.cdate_of_od (Some (Dtext (sub_date ^ end_date)))))
            | None -> burial)
        | _ -> burial
      in
      let pevents = get_pevents p in
      let pevents =
        List.map
          (fun evt ->
            let date = evt.epers_date in
            let date =
              match Date.od_of_cdate date with
              | Some d -> (
                  match d with
                  | Dgreg (_, _) -> date
                  | Dtext t ->
                      if String.length t < 10 then date
                      else
                        let sub_date = String.sub t 0 10 in
                        let end_date = String.sub t 10 (String.length t - 10) in
                        let sub_date = space_to_unders sub_date in
                        Date.cdate_of_od (Some (Dtext (sub_date ^ end_date))))
              | None -> date
            in
            { evt with epers_date = date })
          pevents
      in
      let p =
        { (gen_person_of_person p) with birth; baptism; death; burial; pevents }
      in
      patch_person base p.key_index p)
    (Gwdb.persons base);
  Gwdb.Collection.iter
    (fun fam ->
      let marriage = get_marriage fam in
      let marriage =
        match Date.od_of_cdate marriage with
        | Some d -> (
            match d with
            | Dgreg (_, _) -> marriage
            | Dtext t ->
                if String.length t < 10 then marriage
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Date.cdate_of_od (Some (Dtext (sub_date ^ end_date))))
        | None -> marriage
      in
      let fevents = get_fevents fam in
      let fevents =
        List.map
          (fun evt ->
            let date = evt.efam_date in
            let date =
              match Date.od_of_cdate date with
              | Some d -> (
                  match d with
                  | Dgreg (_, _) -> date
                  | Dtext t ->
                      if String.length t < 10 then date
                      else
                        let sub_date = String.sub t 0 10 in
                        let end_date = String.sub t 10 (String.length t - 10) in
                        let sub_date = space_to_unders sub_date in
                        Date.cdate_of_od (Some (Dtext (sub_date ^ end_date))))
              | None -> date
            in
            { evt with efam_date = date })
          fevents
      in
      let fam = { (gen_family_of_family fam) with marriage; fevents } in
      patch_family base fam.fam_index fam)
    (Gwdb.families base);
  commit_patches base

let bname = ref ""
let speclist = []
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  fix_date_text !bname

let _ = main ()
