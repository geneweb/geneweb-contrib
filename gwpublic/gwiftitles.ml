open Geneweb
open Def

let ind = ref ""
let bname = ref ""
let everybody = ref false

let speclist =
  ["-everybody", Arg.Set everybody, "set flag iftitles to everybody [lent!]";
   "-ind", Arg.String (fun x -> ind := x), "individual key"]
let anonfun i = bname := i
let usage = "Usage: gwiftitles [-everybody] [-ind key] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry
    (Mutil.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  if !everybody then Gwaccess.access_everybody IfTitles !bname
  else Gwaccess.access_some IfTitles !bname !ind

let _ = main ()
