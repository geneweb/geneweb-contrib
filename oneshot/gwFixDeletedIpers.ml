open Geneweb

let () =
  let bname = Sys.argv.(1) in
  Secure.set_base_dir (Filename.dirname bname);
  Lock.control (Mutil.lock_file bname) true ~onerror:Lock.print_try_again @@
  fun () ->
  let open Dbdisk in
  let base = Gwdb_driver_legacy.open_base bname in
  let changes = ref false in
  for i = 0 to base.data.persons.len - 1 do
    let p : (int, int, int) gen_person = base.data.persons.get i in
    if p.key_index <> i
    then begin
      changes := true ;
      Gwdb_driver_legacy.patch_person base i { p with key_index = i } ;
    end ;
  done ;
  if !changes then Gwdb_driver_legacy.commit_patches base ;
