open Def
open Gwdb

let trace = ref false

let update_database_with_burial base =
  let empty_string = Gwdb.insert_string base "" in
  let changed = ref false in
  let nb_modified = ref 0 in
  Gwdb.Collection.iter
    (fun p ->
      match get_burial p with
      | UnknownBurial ->
          if
            sou base (get_burial_place p) = ""
            && sou base (get_burial_src p) = ""
          then ()
          else (
            if !trace then (
              Printf.eprintf "Modifiy person : %s\n" (Gutil.designation base p);
              flush stderr);
            let evt =
              {
                epers_name = Epers_Burial;
                epers_date = Date.cdate_None;
                epers_place = get_burial_place p;
                epers_reason = empty_string;
                epers_note = empty_string;
                epers_src = get_burial_src p;
                epers_witnesses = [||];
              }
            in
            let pevents = get_pevents p @ [ evt ] in
            let gp = { (gen_person_of_person p) with pevents } in
            patch_person base gp.key_index gp;
            changed := true;
            incr nb_modified)
      | _ -> ())
    (Gwdb.persons base);
  if !changed then (
    commit_patches base;
    Printf.eprintf "Number of modified persons: %d\n" !nb_modified;
    flush stderr)

(**/**)

let bname = ref ""
let speclist = [ ("-t", Arg.Set trace, "trace modified person") ]
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  Lock.control (Files.lock_file !bname) false ~onerror:Lock.print_try_again
    (fun () ->
      let base = Gwdb.open_base !bname in
      update_database_with_burial base)

let _ = main ()
