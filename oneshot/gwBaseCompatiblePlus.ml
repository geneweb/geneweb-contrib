open Gwdb

let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: gwBaseCompatiblePlus base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let base = Gwdb.open_base !bname in
  try
    Gwdb.Collection.iter begin fun p ->
      ignore @@ sou base (get_birth_note p) ;
      ignore @@ sou base (get_baptism_note p) ;
      ignore @@ sou base (get_death_note p) ;
      ignore @@ sou base (get_burial_note p)
    end (Gwdb.persons base)
  with _ ->
    Printf.eprintf "GeneWeb base not compatible\n"; flush stderr; exit 2

let _ = main ()






