
(** This executable is used to update the access field of the persons in the
    base to match the confidentiality level assiociated with the given year.

    We only change accesses for persons with default access that do not have
    a date, because when someone has a date, confidentiality is handled by
    geneweb according to the base configuration.

    We perform the task in two steps.
    1) Compute an estimated date for every person in the base when possible
    2) Update the accesses of default access persons accordingly
*)

let nb_years_by_gen = 30
let debug = ref false

(** This module implements a store for the dates of every person in the base.*)
module DatesStore : sig
  type t
  (** the type of the date store *)

  val of_base : Gwdb.base -> t
  (** [of_base base] returns the store corresponding to the base.*)

  val fold : ('a -> iper:Gwdb.iper -> estimated_year:int -> 'a) -> 'a -> t -> 'a
  (** [fold f acc store] fold operation over the store, it only considers the
      persons with an estimated years and ignores the persons with an explicit date in
      their primary events or without a computed date at all.*)
end = struct


  type computed_date =
    | FoundDate of int
    | EstimatedDate of int
    | NoDate

  (* When computing dates for the persons in the base we will need to store the
     state of the computation associated with a person. *)
  type computation =
    | Result of computed_date
    | Ongoing
    | Todo

  let result date = Result date

  let estimated_date year = EstimatedDate year

  let date_of_result = function
    | Result d -> Some d
    | Todo -> None
    | Ongoing -> None

  (* This module implements the actual store of the computation states *)
  module Store : sig
    type t
    val create : Gwdb.iper Gwdb.Collection.t -> Gwdb.ifam Gwdb.Collection.t -> t
    val get : t -> Gwdb.iper -> computation
    val set : t -> Gwdb.iper -> computation -> unit
    val get_siblings : t -> Gwdb.ifam -> Gwdb.iper array option
    val set_siblings : t -> Gwdb.ifam -> Gwdb.iper array option -> unit
    val fold : ('a -> Gwdb.iper -> computation -> 'a) -> 'a -> t -> 'a
  end = struct

    type siblings = Gwdb.iper Array.t

    type t = {
      iper_collection : Gwdb.iper Gwdb.Collection.t;
      iper_store : (Gwdb.iper, computation) Gwdb.Marker.t;
      ifam_store :  (Gwdb.ifam, siblings option) Gwdb.Marker.t;
    }
    (* We keep the collection to ease folding on the store, as markers are not
       foldable *)

    let get {iper_store; _} iper = Gwdb.Marker.get iper_store iper

    let set {iper_store;_} iper comp = Gwdb.Marker.set iper_store iper comp

    let get_siblings {ifam_store; _} ifam = Gwdb.Marker.get ifam_store ifam

    let set_siblings {ifam_store;_} ifam siblings = Gwdb.Marker.set ifam_store ifam siblings

    let create iper_collection ifam_collection =
      let iper_store = Gwdb.iper_marker iper_collection Todo in
      let ifam_store = Gwdb.ifam_marker ifam_collection None in
      {iper_collection; iper_store; ifam_store}

    let fold f acc t =
      Gwdb.Collection.fold (fun acc iper ->
          f acc iper (get t iper)
        ) acc t.iper_collection
  end


  type t = Store.t

  let fold f acc store =
    Store.fold (fun acc iper comp -> match comp with
        | Result (EstimatedDate estimated_year) -> f acc ~iper ~estimated_year
        | Result _ -> acc
        | Ongoing -> assert false (* should not happen *)
        | Todo -> assert false (* should not happen *)
      ) acc store
  (* It is assumed that once the computation is done for the whole base, there should
     not be any ongoing or pending computation. *)

  let is_ongoing store iper =
    Store.get store iper = Ongoing

  let add_not_ongoing_to_queue store iper_queue iper =
    if not (is_ongoing store iper) then
      Queue.add iper iper_queue

  let add_parents_to_queue iper_queue store parents =
    let father = Option.map Gwdb.get_father parents in
    let mother = Option.map Gwdb.get_mother parents in
    Option.iter (add_not_ongoing_to_queue store iper_queue) father;
    Option.iter (add_not_ongoing_to_queue store iper_queue) mother

  let spouses_of_families iper families =
    Array.map (Gutil.spouse iper) families

  let siblings_of_family store parents =
    let ifam = Gwdb.get_ifam parents in
    match Store.get_siblings store ifam with
    | Some siblings -> siblings
    | None ->
      let children = Gwdb.get_children parents in
      Store.set_siblings store ifam (Some children);
      children

  let add_one_gen_to_date = function
    | NoDate as d -> d
    | FoundDate d -> estimated_date (d + nb_years_by_gen)
    | EstimatedDate d -> estimated_date (d + nb_years_by_gen)

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
      Option.map estimated_date (best_date_year_opt d1 d2)
    | Some d, _ | _, Some d ->
      Option.map estimated_date (year_opt_of_date d)
    | None, None -> None

  let best_estimated_date_of_dates d1 d2 =
    let date_opt = Option.map estimated_date (best_date_year_opt d1 d2) in
    Option.value ~default:NoDate date_opt

  let best_date_of_parents store parents =
    let father = Option.map Gwdb.get_father parents in
    let mother = Option.map Gwdb.get_mother parents in
    let date_opt = match father, mother with
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
    Option.value ~default:NoDate date_opt

  let best_date_of_ipers_opt store ipers =
    Array.fold_left (fun best_date iper ->
        let computation = Store.get store iper in
        let date_opt = date_of_result computation in
        let best_year_opt =
          best_date_year_opt best_date (Option.value ~default:NoDate date_opt)
        in
        Option.value ~default:best_date
          (Option.map estimated_date best_year_opt)
      ) NoDate ipers

  (* Traversal of a given node (iper).
     If we find a date in the primary events then we have a result for the node,
     else we have to search through ancestors and relatives, if that is the
     case then we queue them and push the current node on the stack to use the results
     we need when they are available.
  *)
  let rec find_person_date base iper_queue sstack stack (store : t) iper =
    Store.set store iper Ongoing;
    let person = Gwdb.poi base iper in
    match  Gwaccess_util.oldest_year_of person with
      | Some date ->
        Store.set store iper (result (FoundDate date));
        find_person_date_of_queue base iper_queue sstack stack store
      | None ->
        let parents = Option.map (Gwdb.foi base) (Gwdb.get_parents person) in
        add_parents_to_queue iper_queue store parents;
(*        let families = Array.map (Gwdb.foi base) (Gwdb.get_family person) in
        let spouses = spouses_of_families iper families in
          add_not_ongoing_ipers_to_queue iper_queue store spouses;*)
        (*let siblings = Option.map (siblings_of_family store) parents in
          Option.iter (add_not_ongoing_ipers_to_queue iper_queue store) siblings;*)
        Stack.push iper stack;
        find_person_date_of_queue base iper_queue sstack stack store

  (* The stack holds the ids of the nodes that require informations not readily available and
     found during the search. Once the search starting from a node is finished, we can have
     access to the needed values.
  *)
  and compute_stack' base store (stack, list) progress_was_made =
    if Stack.is_empty stack then
      if list = [] then progress_was_made
      else begin
        List.iter (fun iper -> Stack.push iper stack) list;
        progress_was_made
      end
    else
      let iper = Stack.pop stack in
      let person = Gwdb.poi base iper in
      let parents = Option.map (Gwdb.foi base) (Gwdb.get_parents person) in
      let families = Array.map (Gwdb.foi base) (Gwdb.get_family person) in
      let spouses = spouses_of_families iper families in
      let siblings = Option.map (siblings_of_family store) parents in
      let date_parents = best_date_of_parents store parents in
      let date_spouses = best_date_of_ipers_opt store spouses in
      let date_siblings_opt = Option.map (best_date_of_ipers_opt store) siblings in
      let date_siblings = Option.value ~default:NoDate date_siblings_opt in
      let date =
        best_estimated_date_of_dates date_siblings
          (best_estimated_date_of_dates date_parents date_spouses)
      in
      (* Some nodes depend on the results associated to another that in turns depend on their
         own result. This only ever happens if there is a cycle in the graph, or if the node was
         reached by a relative (a spouse or a sibling), but we could not find a result for them.
         We mark these nodes as still pending, because the originating node is in the stack and
         will eventually be associated with a result that we may use, then we will be able to
         break the dependency cycle because all relatives will be either finished or pending but
         not ongoing.
      *)
      let list =
        if date = NoDate then
          iper :: list
        else begin
          Store.set store iper (result date);
          list
        end
      in
      let progress_was_made = progress_was_made || date <> NoDate in
      compute_stack' base store (stack, list) progress_was_made

  and compute_stack base store stack =
    compute_stack' base store (stack, []) false

  and find_person_date_of_queue base iper_queue stack_queue stack store =
    (* Whenever the queue is empty, we finished the search and now have to finalize the
       remaining computations on the stack. *)
    if Queue.is_empty iper_queue then Queue.push stack stack_queue
    else
      let iper = Queue.pop iper_queue in
      match Store.get store iper with
      | Todo -> find_person_date base iper_queue stack_queue stack store iper
      | Result _ -> find_person_date_of_queue base iper_queue stack_queue stack store
      | Ongoing -> find_person_date_of_queue base iper_queue stack_queue stack store

  (* The overall strategy to compute the dates is to perform a breadth-first search for each
     node in the DAG, but never search past an edge more than once by reusing the results of
     previous searches. Each node has a computation state stored in the "store" that helps us
     do just that.
     Given a node in the DAG we need to search for the most recent date we can infer from its
     relatives and ancestors. Breadth-first search is implemented using a queue of the next
     nodes in the DAG. To be tail recursive we need to postpone some of the computations by
     piling the concerned nodes in a stack.
  *)
  let find_person_date base store stack_queue iper =
    let iper_queue = Queue.create () in
    let stack = Stack.create () in
    Queue.add iper iper_queue;
    find_person_date_of_queue base iper_queue stack_queue stack store

  let print_debug_info base store =
    let string_of_date = function
      | Result (FoundDate d) -> Printf.sprintf "found %d" d
      | Result (EstimatedDate d) -> Printf.sprintf "estimated %d" d
      | Result NoDate -> "nodate"
      | Todo -> "todo"
      | Ongoing -> "ongoing"
    in
    Gwdb.Collection.iter (fun iper ->
        let date = Store.get store iper in
        let date_s = string_of_date date in
        let spouses =
          spouses_of_families iper
            (Array.map (Gwdb.foi base) (Gwdb.get_family (Gwdb.poi base iper)))
          |> Array.to_list
        in
        let spouse_dates = List.map (fun iper -> iper, Store.get store iper) spouses in
        let spouses_dates_strings = List.map (fun (iper, d) ->
            Printf.sprintf "<%s> %s" (Gwdb.string_of_iper iper) (string_of_date d)
          ) spouse_dates
        in
        let spouse_str = String.concat "|" spouses_dates_strings in
        print_endline @@ Printf.sprintf "[%s] %s // %s"
          (Gwdb.string_of_iper iper) date_s spouse_str
      ) (Gwdb.ipers base)

  let of_base base =
    let n = Gwdb.nb_of_persons base in
    let ipers_collection = Gwdb.ipers base in
    let ifams_collection = Gwdb.ifams base in
    let store = Store.create ipers_collection ifams_collection in
    let stack_queue = Queue.create () in
    Gwdb.Collection.iteri (fun i iper ->
        find_person_date base store stack_queue iper;
        ProgrBar.run i n
      )
      ipers_collection;
    let rec work_until_no_progress stack_queue nstack_queue progress =
      if Queue.is_empty stack_queue then
        if progress then
          work_until_no_progress nstack_queue stack_queue false
        else begin
          Queue.iter (Stack.iter (fun iper ->
              Store.set store iper (result NoDate)
            )) nstack_queue
        end
      else
        let stack = Queue.pop stack_queue in
        let stack_progress = compute_stack base store stack in
        if not (Stack.is_empty stack) then Queue.push stack nstack_queue;
        let progress = progress || stack_progress in
        work_until_no_progress stack_queue nstack_queue progress
    in
    work_until_no_progress stack_queue (Queue.create ()) false;
    if !debug then print_debug_info base store;
    store
end

let change_access base store lim_year trace =
  DatesStore.fold (fun changes ~iper ~estimated_year ->
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

let compute_persons_accesses ~fast bname lim_year trace =
  let base = Gwdb.open_base bname in
  let () = Gwdb.load_ascends_array base in
  let () = Gwdb.load_couples_array base in
  if fast then Gwdb.load_persons_array base ;
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
  if fast then Gwdb.clear_persons_array base ;
  if changes then Gwdb.commit_patches base;
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
  ; ("-debug", Arg.Set debug, "print debugging information after dates computation")
  ]

let anonfun i = bname := i

let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry
    (Files.lock_file !bname) ~onerror:Lock.print_error_and_exit @@ fun () ->
  compute_persons_accesses ~fast:!fast !bname !lim_year !trace

let _ = main ()
