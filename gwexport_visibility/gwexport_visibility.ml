let today =
  let utm = Unix.time () in
  let tm = Unix.localtime utm in
  {Date.day = tm.Unix.tm_mday; month = succ tm.Unix.tm_mon;
   year = tm.Unix.tm_year + 1900; prec = Sure; delta = 0}

let conf_of_file bname =
  let base_env = Gwd_lib.GwdUtil.read_base_env (Filename.basename bname) in
  let conf = Geneweb.Config.empty in
  { conf with
    private_years = Gwd_lib.GwdUtil.get_private_years base_env;
    allowed_titles = Lazy.from_fun (Gwd_lib.GwdUtil.allowed_titles [] base_env);
    denied_titles = Lazy.from_fun (Gwd_lib.GwdUtil.denied_titles [] base_env);
    public_if_titles = Gwd_lib.GwdUtil.get_public_if_no_date base_env;
    hide_private_names = Gwd_lib.GwdUtil.get_hide_private_names base_env;
    today;
    public_if_no_date = Gwd_lib.GwdUtil.get_public_if_no_date base_env;
    default_contemporary_private_years = Gwd_lib.GwdUtil.get_default_contemporary_private_years base_env;
  }


let format_key ~surname ~firstname ~occ =
  let occ_s = if occ = 0 then "" else Printf.sprintf " %d" occ in
  Printf.sprintf "%s %s%s" surname firstname occ_s

let process bname =
  let base = Gwdb.open_base bname in
  let conf = conf_of_file bname in
  Gwu_lib.prepare_free_occ base;
  Gwdb.Collection.fold (fun l person ->
      let key = Gwu_lib.key_of_person ~format_key base person in
      let visibility =  Geneweb.Util.get_visibility conf base person in
      (key, visibility) :: l
    )
    [] (Gwdb.persons base)

let string_of_visibility = function
  | Geneweb.Util.Visibility_public -> "public"
  | Geneweb.Util.Visibility_semi_public -> "semi_public"
  | Geneweb.Util.Visibility_private -> "private"


let () =
  let bname = ref "" in
  let output_fname = ref "" in
  let anonfun i = bname := i in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " base" in
  let speclist = [
    "--output", Arg.String (fun s -> output_fname := s) , " set file to output the result (by default prints on the std output)."
  ] in
  Arg.parse speclist anonfun usage ;
  match !bname with
  | ""-> Arg.usage speclist usage ;
    exit 2 ;
  | bname ->
    Secure.set_base_dir (Filename.dirname bname);
    Lock.control_retry (Files.lock_file bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
    let res = process bname in

    let oc = open_out !output_fname in
    List.iter (fun (key, visibility) ->
        match key with
        | Some key ->
          let s = Printf.sprintf "%s %s\n" key (string_of_visibility visibility) in
          output_string oc s
        | None -> ()
      ) res;
    close_out oc


