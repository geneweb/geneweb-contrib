open Def
open Gwdb

let nb_years_by_gen = 30

module DatesStore : sig
  type t

  type computed_date

  val of_base : Gwdb.base -> t

  val fold : ('a -> iper:Gwdb.iper -> estimated_year:int -> 'a) -> 'a -> t -> 'a

end = struct


  type computed_date =
    | FoundDate of int
    | EstimatedDate of int
    | NoDate

  type computation =
    | Result of computed_date
    | Ongoing
    | Todo

  let result date = Result date

  let date_of_result = function
      Result d -> Some d
    | Todo -> None
    | Ongoing -> None

  module Store : sig
    type t
    val create : Gwdb.iper Gwdb.Collection.t -> t
    val get : t -> Gwdb.iper -> computation
    val set : t -> Gwdb.iper -> computation -> unit
    val fold : ('a -> iper -> computation -> 'a) -> 'a -> t -> 'a
  end = struct

    type t = {
      collection : Gwdb.iper Gwdb.Collection.t;
      store : (Gwdb.iper, computation) Gwdb.Marker.t
    }

    let get {store; _} iper = Gwdb.Marker.get store iper

    let set {store;_} iper comp = Gwdb.Marker.set store iper comp

    let create collection =
      let store = Gwdb.iper_marker collection Todo in
      {collection; store}

    let fold f acc t =
      Gwdb.Collection.fold (fun acc iper ->
          f acc iper (get t iper)
        ) acc t.collection
  end


  type t = Store.t

  let fold f acc store =
    Store.fold (fun acc iper comp -> match comp with
        | Result (EstimatedDate estimated_year) -> f acc ~iper ~estimated_year
        | Result _ -> acc
        | Ongoing -> assert false
        | Todo -> assert false
      ) acc store

  let is_ongoing store iper =
    Store.get store iper = Ongoing

  let add_parents_to_queue iper_queue store parents =
    let father = Option.map Gwdb.get_father parents in
    let mother = Option.map Gwdb.get_mother parents in
    Option.iter (fun iper ->
        if not (is_ongoing store iper) then
          Queue.add iper iper_queue
      ) father;
    Option.iter (fun iper ->
        if not (is_ongoing store iper) then
          Queue.add iper iper_queue
      ) mother

  let spouses_of_families iper families =
    let get_spouse iper family =
      let fath = Gwdb.get_father family in
      let moth = Gwdb.get_mother family in
      if Gwdb.compare_iper iper fath = 0 then moth else fath
    in
    Array.map (get_spouse iper) families

  let add_spouses_to_queue iper_queue store spouses =
    Array.iter (fun iper ->
        if not (is_ongoing store iper) then
          Queue.add iper iper_queue
      ) spouses

  let add_one_gen_to_date = function
    | NoDate as d -> d
    | FoundDate d -> EstimatedDate (d + nb_years_by_gen)
    | EstimatedDate d -> EstimatedDate (d + nb_years_by_gen)

  let best_date_year_opt d1 d2 = match d1, d2 with
    | FoundDate d1, FoundDate d2
    | EstimatedDate d1, EstimatedDate d2 ->
      if d1 > d2 then Some d1 else Some d2
    | FoundDate d, _
    | _, FoundDate d
    | EstimatedDate d, NoDate
    | NoDate, EstimatedDate d ->
      Some d
    | NoDate, NoDate -> None

  let year_opt_of_date = function
      FoundDate d -> Some d
    | EstimatedDate d -> Some d
    | NoDate -> None

  let best_estimated_date_of_years d1 d2 = match d1, d2 with
    | Some d1, Some d2 ->
      Option.map (fun d -> EstimatedDate d) (best_date_year_opt d1 d2)
    | Some d, _ | _, Some d ->
      Option.map (fun d -> EstimatedDate d) (year_opt_of_date d)
    | None, None -> None

  let best_date_of_parents store parents =
    let father = Option.map Gwdb.get_father parents in
    let mother = Option.map Gwdb.get_mother parents in
    let date = match father, mother with
      | Some ifath, Some imoth ->
        let ifath_year_opt = date_of_result (Store.get store ifath) in
        let imoth_year_opt = date_of_result (Store.get store imoth) in
        let date_opt = best_estimated_date_of_years ifath_year_opt imoth_year_opt in
        Option.map add_one_gen_to_date date_opt
      | Some ifath, None ->
        let d = date_of_result (Store.get store ifath) in
        Option.map add_one_gen_to_date d
      | None, Some imoth ->
        let d = date_of_result (Store.get store imoth) in
        Option.map add_one_gen_to_date d
      | None, None -> None
    in
    if Option.is_none date then NoDate else Option.get date

  let best_date_of_spouses store spouses =
    let date = Array.fold_left (fun best_date iper ->
        let spouse_computation = Store.get store iper in
        let spouse_date = date_of_result spouse_computation in
        match best_date_year_opt best_date (Option.value ~default:NoDate spouse_date) with
        | Some d -> EstimatedDate d
        | None -> best_date
      ) NoDate spouses
    in
    date

  let rec find_person_date base iper_queue stack (store : t) iper =
    Store.set store iper Ongoing;
    let person = Gwdb.poi base iper in
    begin match  Gwaccess_util.oldest_year_of person with
      | Some date ->
        Store.set store iper (result (FoundDate date));
        find_person_date_of_queue base iper_queue stack store
      | None ->
        let parents = Option.map (Gwdb.foi base) (Gwdb.get_parents person) in
        add_parents_to_queue iper_queue store parents;
        let families = Array.map (Gwdb.foi base) (Gwdb.get_family person) in
        let spouses = spouses_of_families iper families in
        add_spouses_to_queue iper_queue store spouses;
        Stack.push iper stack;
        find_person_date_of_queue base iper_queue stack store
    end

  and compute_stack base store stack =
    if Stack.is_empty stack then ()
    else
      let iper = Stack.pop stack in
      let person = Gwdb.poi base iper in
      let parents = Option.map (Gwdb.foi base) (Gwdb.get_parents person) in
      let families = Array.map (Gwdb.foi base) (Gwdb.get_family person) in
      let spouses = spouses_of_families iper families in
      let date_parents = best_date_of_parents store parents in
      let date_spouses = best_date_of_spouses store spouses in
      let date_year_opt = best_date_year_opt date_spouses date_parents in
      let date_opt = Option.map (fun year -> EstimatedDate year) date_year_opt in
      let date = Option.value ~default:NoDate date_opt in
      let has_ongoing_spouse =
        Array.exists (fun iper -> Store.get store iper = Ongoing) spouses
      in
      if has_ongoing_spouse && date = NoDate then
        Store.set store iper Todo
      else
        Store.set store iper (result date);
      compute_stack base store stack

  and find_person_date_of_queue base iper_queue stack store =
    if Queue.is_empty iper_queue then compute_stack base store stack
    else
      let iper = Queue.pop iper_queue in
      match Store.get store iper with
      | Todo -> find_person_date base iper_queue stack store iper
      | Result _ -> find_person_date_of_queue base iper_queue stack store
      | Ongoing -> find_person_date_of_queue base iper_queue stack store

  let find_person_date base store iper =
    let iper_queue = Queue.create () in
    let stack = Stack.create () in
    Queue.add iper iper_queue;
    find_person_date_of_queue base iper_queue stack store

  let _debug base store =
    let string_of_date = function
      | Result (FoundDate d) -> Printf.sprintf "found %d" d
      | Result (EstimatedDate d) -> Printf.sprintf "estimated %d" d
      | Result NoDate -> "nodate"
      | Todo -> assert false
      | Ongoing -> assert false
    in
    Gwdb.Collection.iter (fun iper ->
        let date = Store.get store iper in
        let date_s = string_of_date date in

        let spouses = spouses_of_families iper (Array.map (Gwdb.foi base) (Gwdb.get_family (Gwdb.poi base iper))) in
        let spouse_dates = Array.map (fun iper -> iper, Store.get store iper) spouses in
        let spouses_dates_strings = Array.to_list (Array.map (fun (iper, d) ->
            Printf.sprintf "<%s> %s" (Gwdb.string_of_iper iper) (string_of_date d)) spouse_dates) in
        let spouse_str = String.concat "|" spouses_dates_strings in

        print_endline @@ Printf.sprintf "[%s] %s // %s" (Gwdb.string_of_iper iper) date_s spouse_str
      ) (Gwdb.ipers base)

  let of_base base =
    let n = nb_of_persons base in
    let ipers_collection = Gwdb.ipers base in
    let store = Store.create ipers_collection in
    Gwdb.Collection.iteri (fun i iper ->
        find_person_date base store iper;
        ProgrBar.run i n
      )
      ipers_collection;
    (*debug base store;*)
    store
end

let change_access base store lim_year trace =
  DatesStore.fold (fun changes ~iper ~estimated_year ->
      (*print_endline @@ Printf.sprintf "<%s> %s" (Gwdb.string_of_iper iper) (string_of_int estimated_year);*)
      let p = Gwdb.poi base iper in
      match Gwdb.get_access p with
      | IfTitles ->
        let access =
          if estimated_year > lim_year then Def.Private
          else Def.Public
        in
        let gen_person = {(Gwdb.gen_person_of_person p) with access = access} in
        Gwdb.patch_person base gen_person.key_index gen_person;
        if trace then begin
          let access_string = if access = Def.Private then "private" else "public" in
          let str = Printf.sprintf
              "%s -> %s (found an estimated year of %d)"
              (Gutil.designation base p) access_string estimated_year
          in
          print_endline str
        end;
        true
      | Def.Private
      | Def.Public -> changes
    ) false store

let change_somebody_access base lim_year trace p year_of_p spouse =
  if year_of_p = None && (get_access p = IfTitles || spouse) then
    match Gwaccess_util.find_dated_ancestor base p with
      Some (a, year, nb_gen) ->
        let acc =
          if year + nb_gen * nb_years_by_gen > lim_year then Private
          else Public
        in
        let gp = {(gen_person_of_person p) with access = acc} in
        patch_person base gp.key_index gp;
        if trace then
          begin
            Printf.printf "%s -> " (Gutil.designation base p);
            if acc = Private then Printf.printf "private" else Printf.printf "public";
            Printf.printf " (anc %d gen %s year %d)" nb_gen
              (Gutil.designation base a) year;
            Printf.printf "\n";
            flush stdout
          end;
        Some acc
    | None -> None
  else None

let public_all ~fast bname lim_year trace =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let _n = nb_of_persons base in
  if fast then load_persons_array base ;
  Consang.check_noloop base
    (function
       OwnAncestor p ->
         Printf.printf "I cannot deal this database.\n";
         Printf.printf "%s is his own ancestors\n" (Gutil.designation base p);
         flush stdout;
         exit 2
     | _ -> assert false);
  ProgrBar.start ();
  let store = DatesStore.of_base base in
  let changes = change_access base store lim_year trace in
  if fast then clear_persons_array base ;
  if changes then commit_patches base;
  ProgrBar.finish ()

let lim_year = ref 1900
let trace = ref false
let bname = ref ""
let fast = ref false

let speclist =
  [ ("-fast", Arg.Set fast, " fast mode. Needs more memory.")
  ; ("-y", Arg.Int (fun i -> lim_year := i),
     "limit year (default = " ^ string_of_int !lim_year ^ ")")
  ; ("-t", Arg.Set trace, "trace changed persons")
  ]

let anonfun i = bname := i

let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry
    (Files.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  public_all ~fast:!fast !bname !lim_year !trace

let _ = main ()
