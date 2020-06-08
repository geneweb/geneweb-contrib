open Geneweb

type person = Dbdisk.dsk_person
type ascend = Dbdisk.dsk_ascend
type union = Dbdisk.dsk_union
type family = Dbdisk.dsk_family
type couple = Dbdisk.dsk_couple
type descend = Dbdisk.dsk_descend

(* Copied from gwdb-legacy/database.ml *)
type patches_ht =
  { h_person : int ref * (int, person) Hashtbl.t;
    h_ascend : int ref * (int, ascend) Hashtbl.t;
    h_union : int ref * (int, union) Hashtbl.t;
    h_family : int ref * (int, family) Hashtbl.t;
    h_couple : int ref * (int, couple) Hashtbl.t;
    h_descend : int ref * (int, descend) Hashtbl.t;
    h_string : int ref * (int, string) Hashtbl.t;
    h_name : (int, int list) Hashtbl.t
  }

let () =
  let bname = Sys.argv.(1) in
  Secure.set_base_dir (Filename.dirname bname);
  Lock.control (Mutil.lock_file bname) true ~onerror:Lock.print_try_again @@
  fun () ->
  let open Dbdisk in
  let base = Gwdb_driver_legacy.open_base bname in
  let bdir = base.data.bdir in
  let ic = Secure.open_in_bin (Filename.concat bdir "patches") in
  assert (Mutil.check_magic "GnPa0001" ic) ;
  let ht : patches_ht = input_value ic in
  let changes = ref false in
  let aux ht =
    let n = fst ht in
    let ht = snd ht in
    let new_ht = Hashtbl.create 0 in
    Hashtbl.iter begin fun k v ->
      if k <> -1 then begin
        changes := true ;
        Hashtbl.add new_ht k v ;
      end ;
    end ht ;
    (n, new_ht)
  in
  let ht' =
    { h_person = aux ht.h_person
    ; h_ascend = aux ht.h_ascend
    ; h_union = aux ht.h_union
    ; h_family = aux ht.h_family
    ; h_couple = aux ht.h_couple
    ; h_descend = aux ht.h_descend
    ; h_string = aux ht.h_string
    ; h_name = ht.h_name
    }
  in
  if !changes then begin
    let tmp_fname = Filename.concat bdir "1patches" in
    let fname = Filename.concat bdir "patches" in
    let oc9 = Secure.open_out_bin tmp_fname in
    output_string oc9 "GnPa0001" ;
    Dutil.output_value_no_sharing oc9 (ht' : patches_ht) ;
    close_out oc9;
    Mutil.rm (fname ^ "~");
    Mutil.mv fname (fname ^ "~") ;
    Mutil.mv tmp_fname fname ;
    print_endline @@ bname ^ ": fixed patch file"
  end else
    print_endline bname
