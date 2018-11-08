open Geneweb
open Gwdb
open Json

let npoc_of_person base person =
  let firstname =  sou base (get_first_name person) in
  let lastname =  sou base (get_surname person) in
  let n = Name.lower lastname in
  let p = Name.lower firstname in
  let occ = get_occ person in
    Printf.sprintf "%s:%s:%d" n p occ


let key_of_person basename person =
  basename ^ ":" ^ string_of_iper (get_iper person)

let print_person base basename person =
  let json_person = json_of_person base person in
  let key = key_of_person basename person in
  let json = `Assoc [
    ("_key", `String key);
    ("basename", `String basename);
    ("person", json_person);
  ] in
  Printf.printf "%s\n" (Yojson.to_string json)

let print_parents base basename person =
  match get_parents person with
  | None -> ()
  | Some ifam -> let fam = foi base ifam in
      let father = poi base (get_father fam) in
      let mother = poi base (get_mother fam) in
      let person_id = Printf.sprintf "persons/%s" (key_of_person basename person) in
      let father_id = Printf.sprintf "persons/%s" (key_of_person basename father) in
      let mother_id = Printf.sprintf "persons/%s" (key_of_person basename mother) in
      let json_father = `Assoc [ ("_from", `String father_id); ("_to", `String person_id); ("basename", `String basename) ] in
      let json_mother = `Assoc [ ("_from", `String mother_id); ("_to", `String person_id); ("basename", `String basename)] in
        Printf.printf "%s\n%s\n" (Yojson.Basic.to_string json_father)  (Yojson.Basic.to_string json_mother)

let print_family base basename family =
  let key = Printf.sprintf "%s:%s" basename (string_of_ifam @@ get_ifam family) in
  let father = poi base (get_father family) in
  let mother = poi base (get_mother family) in
  let father_id = Printf.sprintf "persons/%s" (key_of_person basename father) in
  let mother_id = Printf.sprintf "persons/%s" (key_of_person basename mother) in
  let json_family = json_of_family base family in
  let json_edge =
    `Assoc [ ("_from", `String father_id)
           ; ("_to", `String mother_id)
           ; ("family", json_family)
           ; ("basename", `String basename)
           ; ("_key", `String key) ]
  in
  Printf.printf "%s\n" (Yojson.to_string json_edge)

let print_json basename mode =
  let base = Gwdb.open_base basename in
  match mode with
  | "persons" -> Gwdb.Collection.iter (print_person base basename) (Gwdb.persons base)
  | "parents" -> Gwdb.Collection.iter (print_parents base basename) (Gwdb.persons base)
  | "families" -> Gwdb.Collection.iter (print_family base basename) (Gwdb.families base)
  | _ -> ()

let bname = ref ""
let mode = ref ""

let set_mode mode_work = mode := mode_work
let anonfun i = bname := i

let speclist = [
  ("-mode", Arg.String (set_mode), "the mode to work in : persons|parents|families");
]

let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control_retry (Mutil.lock_file !bname) ~onerror:Lock.print_error_and_exit @@
  fun () -> print_json !bname !mode

let _ = main ()
