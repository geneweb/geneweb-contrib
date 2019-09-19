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

let aux_istr_person fn p : dsk_person =
  { p with
    first_name = fn p.first_name
  ; surname = fn p.surname
  ; image = fn p.image
  ; public_name = fn p.public_name
  ; qualifiers = List.map fn p.qualifiers
  ; aliases = List.map fn p.aliases
  ; first_names_aliases = List.map fn p.first_names_aliases
  ; surnames_aliases = List.map fn p.surnames_aliases
  ; titles = List.map begin fun t ->
      { t with t_name = (match t.t_name with Tname i -> Tname (fn i) | x -> x)
             ; t_ident = fn t.t_ident
             ; t_place = fn t.t_place
      } end p.titles
  ; occupation = fn p.occupation
  ; birth_place = fn p.birth_place
  ; birth_note = fn p.birth_note
  ; birth_src = fn p.birth_src
  ; baptism_place = fn p.baptism_place
  ; baptism_note = fn p.baptism_note
  ; baptism_src = fn p.baptism_src
  ; death_place = fn p.death_place
  ; death_note = fn p.death_note
  ; death_src = fn p.death_src
  ; burial_place = fn p.burial_place
  ; burial_note = fn p.burial_note
  ; burial_src = fn p.burial_src
  ; notes = fn p.notes
  ; psources = fn p.psources
  ; rparents = List.map (fun r -> { r with r_sources = fn r.r_sources} ) p.rparents
  ; pevents = List.map begin fun e ->
      { e with
        epers_name = (match e.epers_name with Epers_Name i -> Epers_Name (fn i) | x -> x)
      ; epers_place = fn e.epers_place
      ; epers_reason = fn e.epers_reason
      ; epers_note = fn e.epers_note
      ; epers_src = fn e.epers_src
      } end p.pevents
  }

let aux_istr_family fn f =
  { f with
    marriage_place = fn f.marriage_place
  ; marriage_note = fn f.marriage_note
  ; marriage_src = fn f.marriage_src
  ; comment = fn f.comment
  ; origin_file = fn f.origin_file
  ; fsources = fn f.fsources
  ; fevents = List.map begin fun e ->
      { e with
        efam_name = (match e.efam_name with Efam_Name i -> Efam_Name (fn i) | x -> x)
      ; efam_place = fn e.efam_place
      ; efam_reason = fn e.efam_reason
      ; efam_note = fn e.efam_note
      ; efam_src = fn e.efam_src
      } end f.fevents
  }

let scan base string person family =
  let mark t i = Array.unsafe_set t i true ; i in
  if !step_strings && base.data.strings.len > 2 then begin
    load_array base.data.strings ;
    let rev = Hashtbl.create base.data.strings.len in
    for i = 0 to base.data.strings.len - 1 do
      Hashtbl.add rev (base.data.strings.get i) i
    done ;
    let opt_mark t s = match Hashtbl.find_opt rev s with Some x -> ignore @@ mark t x | None -> () in
    let t = Array.make base.data.strings.len false in
    Array.unsafe_set t 0 true ;
    Array.unsafe_set t 1 true ;
    load_array base.data.persons ;
    for i = 0 to base.data.persons.len - 1 do
      let p = base.data.persons.get i in
      List.iter (opt_mark t) (split_fname base p.first_name) ;
      List.iter (opt_mark t) (split_sname base p.surname) ;
      opt_mark t @@ Name.concat (base.data.strings.get p.first_name) (base.data.strings.get p.surname) ;
      ignore @@ aux_istr_person (mark t) p
    done ;
    clear_array base.data.persons ;
    load_array base.data.families ;
    for i = 0 to base.data.families.len - 1 do
      ignore @@ aux_istr_family (mark t) (base.data.families.get i)
    done ;
    clear_array base.data.families ;
    clear_array base.data.strings ;
    string base t
  end ;
  if !step_persons then begin
    load_array base.data.persons ;
    let t = Array.make base.data.persons.len false in
    for i = 0 to base.data.persons.len - 1 do
      if (base.data.persons.get i).key_index <> Gwdb1.dummy_iper then ignore @@ mark t i
    done ;
    clear_array base.data.persons ;
    person base t
  end ;
  if !step_families then begin
    load_array base.data.families ;
    let t = Array.make base.data.families.len false in
    for i = 0 to base.data.families.len - 1 do
      if (base.data.families.get i).fam_index <> Gwdb1.dummy_ifam then ignore @@ mark t i
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

(* Do not clear modified arrays, or modifications would be lost *)
let compact base =
  let compact_istr base t =
    base.data.strings.clear_array () ;
    base.data.strings.load_array () ;
    let rec loop acc i =
      if i = -1 then acc
      else if not t.(i) then loop (i :: acc) (i - 1)
      else loop acc (i - 1)
    in
    let acc = loop [] (Array.length t - 1) in
    let shift i =
      let rec loop acc = function
        | j :: tl when j < i -> loop j tl
        | _ -> acc
      in loop 0 acc
    in
    for i = 0 to base.data.strings.len - 1 do
      match shift i with
      | 0 -> ()
      | shift -> base.data.strings.set (i - shift) (base.data.strings.get i)
    done ;
    base.data.persons.load_array () ;
    for i = 0 to base.data.persons.len - 1 do
      let p = base.data.persons.get i in
      let p' = aux_istr_person (fun i -> i - shift i) p in
      if p <> p' then begin
        Printf.printf "Person changed: %d\n" i ;
        base.data.persons.set i p'
      end
    done
  in
  scan base compact_istr (fun _ _ -> ()) (fun _ _ -> ())

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
    , " dump unused values" )
  ; ( "-compact"
    , Arg.Unit (fun () -> action := Some `compact)
    , " compact database and remove unused values (only dry run is implemented)" )
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
  | Some `compact -> compact base
