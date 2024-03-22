open Def
open Gwdb

let nb_years_by_gen = 30

let change_somebody_access base lim_year trace p year_of_p =
  if year_of_p = None && get_access p = IfTitles then
    match Gwaccess_util.find_dated_ancestor base p with
      Some (a, year, nb_gen) ->
        let acc =
          if year + nb_gen * nb_years_by_gen > lim_year then IfTitles
          else Public
        in
        let gp = {(gen_person_of_person p) with access = acc} in
        patch_person base gp.key_index gp;
        if trace && acc <> IfTitles then
          begin
            Printf.printf "%s -> " (Gutil.designation base p);
            if acc = Private then Printf.printf "private" else Printf.printf "public";
            Printf.printf " (anc %d gen %s year %d)" nb_gen
              (Gutil.designation base a) year;
            Printf.printf "\n";
            flush stdout;
            Some acc
          end
        else None
    | None -> None
  else None

let public_all bname lim_year trace =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  Consang.check_noloop base
    (function
       OwnAncestor p ->
         Printf.printf "I cannot deal this database.\n";
         Printf.printf "%s is his own ancestors\n" (Gutil.designation base p);
         flush stdout;
         exit 2
     | _ -> assert false);
  let n = nb_of_persons base in
  let changes = ref false in
  ProgrBar.start ();
  Gwdb.Collection.iteri begin fun i p ->
    ProgrBar.run i n;
    if Gwaccess_util.oldest_year_of p = None && get_access p = IfTitles then
      match change_somebody_access base lim_year trace p (Gwaccess_util.oldest_year_of p) with
      | Some _ -> changes := true
      | None ->
          let fama = get_family p in
          let rec loop i =
            if i = Array.length fama then ()
            else
              let ifam = fama.(i) in
              let isp = Gutil.spouse (get_iper p) (foi base ifam) in
              let sp = poi base isp in
              let year_of_sp = Gwaccess_util.oldest_year_of sp in
              let acc_opt =
                match year_of_sp with
                  Some year ->
                    Some (if year > lim_year then IfTitles else Public)
                | None ->
                    change_somebody_access base lim_year trace sp year_of_sp
              in
              match acc_opt with
                Some acc ->
                  let gp = {(gen_person_of_person p) with access = acc} in
                  patch_person base gp.key_index gp;
                  changes := true;
                  if trace && acc <> IfTitles then
                    begin
                      Printf.printf "%s -> " (Gutil.designation base p);
                      if acc = Private then Printf.printf "private"
                      else Printf.printf "public";
                      Printf.printf " (inherited from spouse %s)"
                        (Gutil.designation base sp);
                      Printf.printf "\n";
                      flush stdout
                    end
              | None -> loop (i + 1)
          in
          loop 0
  end (Gwdb.persons base) ;
  if !changes then commit_patches base;
  ProgrBar.finish ()

let lim_year = ref 1900
let trace = ref false
let bname = ref ""

let speclist =
  ["-y", Arg.Int (fun i -> lim_year := i),
   "limit year (default = " ^ string_of_int !lim_year ^ ")";
   "-t", Arg.Set trace, "trace changed persons"]
let anonfun i = bname := i
let usage = "Usage: public [-y #] [-t] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname) ;
  Lock.control_retry
    (Files.lock_file !bname)
    ~onerror:Lock.print_error_and_exit
    (fun () -> public_all !bname !lim_year !trace)

let _ = main ()
