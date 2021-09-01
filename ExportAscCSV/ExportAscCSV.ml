let open Gwdb in

let escape_quote s =
  let b = Buffer.create (String.length s) in
  String.iter begin function
    | '\\' -> Buffer.add_string b "\\\\"
    | '"' -> Buffer.add_string b "\\\""
    | '\000'..'\031' -> Buffer.add_char b '_'
    | c -> Buffer.add_char b c
  end s ;
  Buffer.contents b
in

(* TODO : tableau de mark pour les implexes. *)
let print_asc_csv base ip nb_gen =
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let p = poi base ip in
  let nb_person = 1 in
  let rec loop i nb_person p =
    if i = nb_gen then ()
    else
      let (birth, death, _) = "", "", "" in
      Printf.fprintf stdout "\"%d\";\"%s\";\"%s\";\"%s\";\"%s\";\n" nb_person
        (escape_quote (sou base (get_surname p)))
        (escape_quote (sou base (get_first_name p))) birth death;
      match get_parents p with
        Some ifam ->
        let cpl = foi base ifam in
        loop (succ i) (nb_person * 2) (poi base (get_father cpl));
        loop (succ i) (nb_person * 2 + 1) (poi base (get_mother cpl))
      | None -> ()
  in
  loop 0 nb_person p
in

let ind = ref "" in
let nb_gen = ref 4 in
let bname = ref "" in

let speclist =
  [ "-i", Arg.Set_string ind, " ip of the person (default = 0)"
  ; "-n", Arg.Set_int nb_gen
    , " number of generation (default = " ^ string_of_int !nb_gen ^ ")"
  ] |> Arg.align
in
let anonfun s = assert (!bname = "") ; bname := s in
let usage =
  "Usage: cat ExportAscCSV | GWREPL_NOPROMT=1 GWREPL_PPF=/dev/null " ^ Sys.argv.(0) ^ " [-i #] [-n #] base"
in

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname) ;
  print_asc_csv (Gwdb.open_base !bname) (Gwdb.iper_of_string !ind) !nb_gen
in

main ()
;;
