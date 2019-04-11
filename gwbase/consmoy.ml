(* Copyright (c) 2000 INRIA *)

open Geneweb
open Gwdb

let consmoy base =
  let sum =
    Gwdb.Collection.fold
      (fun acc p -> acc +. Adef.float_of_fix (get_consang p))
      0.0 (Gwdb.persons base)
  in
  Printf.printf "average consanguinity: %f\n" (sum /. float (nb_of_persons base));
  flush stdout;
  ()

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  Argl.parse speclist (fun s -> bname := s) usage;
  let base = Gwdb.open_base !bname in consmoy base

let _ = Printexc.print main ()
