(** WARNING

    Make sure that theses predicates are true

    type istr = int
    type ifam = int
    type iper = int
    type base = Dbdisk.dsk_base
*)

let () =
  let bname = Sys.argv.(1) in
  Secure.set_base_dir (Filename.dirname bname);
  Lock.control (Files.lock_file bname) true ~onerror:Lock.print_try_again @@
  fun () ->
  let open Dbdisk in
  let base = Database.opendb bname in
  let changes = ref 0 in
  base.data.persons.load_array () ;
  for i = 0 to base.data.persons.len - 1 do
    let p : (int, int, int) gen_person = base.data.persons.get i in
    if p.key_index <> i
    then begin
      incr changes ;
      Gwdb_driver.patch_person (Obj.magic base) (Obj.magic i) (Obj.magic { p with key_index = i }) ;
    end ;
  done ;
  if !changes <> 0 then begin
    Gwdb_driver.commit_patches (Obj.magic base) ;
    print_endline @@ "Commited " ^ string_of_int !changes ^ " changes."
  end else
    print_endline "No changes commited."
