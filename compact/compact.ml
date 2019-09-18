open Geneweb
open Def
open Dbdisk

let fast = ref true
let step_strings = ref false
let step_persons = ref false
let step_families = ref false

let load_array a = if !fast then a.load_array ()
let clear_array a = a.clear_array ()

let split_sname base i = Mutil.split_sname @@ base.data.strings.get i
let split_fname base i = Mutil.split_fname @@ base.data.strings.get i

let aux_istr_person fn p =
  fn p.first_name ;
  fn p.surname ;
  fn p.image ;
  fn p.public_name ;
  List.iter fn p.qualifiers ;
  List.iter fn p.aliases ;
  List.iter fn p.first_names_aliases ;
  List.iter fn p.surnames_aliases ;
  List.iter begin fun {t_name;t_ident;t_place; _ } ->
    fn t_ident ;
    fn t_place ;
    (match t_name with Tname i -> fn i | _ -> ()) ;
  end p.titles ;
  fn p.occupation ;
  fn p.birth_place ;
  fn p.birth_note ;
  fn p.birth_src ;
  fn p.baptism_place ;
  fn p.baptism_note ;
  fn p.baptism_src ;
  fn p.death_place ;
  fn p.death_note ;
  fn p.death_src ;
  fn p.burial_place ;
  fn p.burial_note ;
  fn p.burial_src ;
  fn p.notes ;
  fn p.psources ;
  List.iter (fun {r_sources;_} -> fn r_sources) p.rparents ;
  List.iter begin fun {epers_name;epers_place;epers_reason;epers_note;epers_src;_} ->
    (match epers_name with Epers_Name i -> fn i | _ -> ()) ;
    fn epers_place ;
    fn epers_reason ;
    fn epers_note ;
    fn epers_src ;
  end p.pevents

let scan base string person family =
  let mark t i = Array.unsafe_set t i true in
  if !step_strings && base.data.strings.len > 2 then begin
    load_array base.data.strings ;
    let rev = Hashtbl.create base.data.strings.len in
    for i = 0 to base.data.strings.len - 1 do
      Hashtbl.add rev (base.data.strings.get i) i
    done ;
    let opt_mark t s = match Hashtbl.find_opt rev s with Some x -> mark t x | None -> () in
    let t = Array.make base.data.strings.len false in
    Array.unsafe_set t 0 true ;
    Array.unsafe_set t 1 true ;
    load_array base.data.persons ;
    for i = 0 to base.data.persons.len - 1 do
      let p = base.data.persons.get i in
      List.iter (opt_mark t) (split_fname base p.first_name) ;
      List.iter (opt_mark t) (split_sname base p.surname) ;
      opt_mark t @@ Name.concat (base.data.strings.get p.first_name) (base.data.strings.get p.surname) ;
      aux_istr_person (mark t) p
    done ;
    clear_array base.data.persons ;
    load_array base.data.families ;
    for i = 0 to base.data.families.len - 1 do
      let f = base.data.families.get i in
      mark t f.marriage_place ;
      mark t f.marriage_note ;
      mark t f.marriage_src ;
      mark t f.comment ;
      mark t f.origin_file ;
      mark t f.fsources ;
      List.iter begin fun {efam_name;efam_place;efam_reason;efam_note;efam_src;_} ->
        (match efam_name with Efam_Name i -> mark t i | _ -> ()) ;
        mark t efam_place ;
        mark t efam_reason ;
        mark t efam_note ;
        mark t efam_src ;
      end f.fevents
    done ;
    clear_array base.data.families ;
    clear_array base.data.strings ;
    string base t
  end ;
  if !step_persons then begin
    load_array base.data.persons ;
    let t = Array.make base.data.persons.len false in
    for i = 0 to base.data.persons.len - 1 do
      if (base.data.persons.get i).key_index <> Gwdb1.dummy_iper then mark t i
    done ;
    clear_array base.data.persons ;
    person base t
  end ;
  if !step_families then begin
    load_array base.data.families ;
    let t = Array.make base.data.families.len false in
    for i = 0 to base.data.families.len - 1 do
      if (base.data.families.get i).fam_index <> Gwdb1.dummy_ifam then mark t i
    done ;
    clear_array base.data.families ;
    family base t
  end

let report base =
  let aux fn  _base t =
    let cnt = ref 0 in
    for i = 0 to Array.length t - 1 do if not t.(i) then incr cnt done ;
    fn !cnt
  in
  scan
    base
    (aux @@ fun c -> Printf.printf "Number of unused strings: %d\n" c)
    (aux @@ fun c -> Printf.printf "Number of ghost persons: %d\n" c)
    (aux @@ fun c -> Printf.printf "Number of ghost families: %d\n" c)

let dump base =
  let dump_istr base t =
    for i = 0 to Array.length t - 1 do
      if not t.(i) then begin
        Printf.printf "=== [START ISTR %d] ===\n%s\n=== [END ISTR %d] ===\n\n" i (base.data.strings.get i) i
      end
    done
  in
  let dump_iper _base t =
    for i = 0 to Array.length t - 1 do
      if not t.(i) then begin Printf.printf "Ghost person: %d\n" i end
    done
  in
  let dump_ifam _base t =
    for i = 0 to Array.length t - 1 do
      if not t.(i) then begin Printf.printf "Ghost family: %d\n" i end
    done
  in
  scan base dump_istr dump_iper dump_ifam

let steps = ref "strings,persons,families"
let bname = ref ""
let action = ref None
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] ACTIOn base"

let speclist =
  [ ( "-mem"
    , Arg.Clear fast
    , " slower, but use less memory" )
  ; ( "-step"
    , Arg.Set_string steps
    , " STEPS steps to perform. Default is " ^ !steps )
  ; ( "-report"
    , Arg.Unit (fun () -> action := Some `report)
    , " only report number of unused values" )
  ; ( "-dump"
    , Arg.Unit (fun () -> action := Some `dump)
    , " dump unuse values" )
  ]

let _ =
  Arg.parse speclist (fun s -> bname := s) usage ;
  Secure.set_base_dir (Filename.dirname !bname) ;
  if !bname = "" then begin Arg.usage speclist usage ; exit 2 end;
  List.iter begin function
    | "strings" -> step_strings := true
    | "persons" -> step_persons := true
    | "families" -> step_families := true
    | _ -> Arg.usage speclist usage ; exit 2
  end (String.split_on_char ',' !steps) ;
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again @@ fun () ->
  let base = Gwdb1.OfGwdb.base (Gwdb.open_base !bname) in
  match !action with
  | None -> Arg.usage speclist usage ; exit 2
  | Some `dump -> dump base
  | Some `report -> report base
