
module IstrSet = Set.Make (struct type t = Gwdb.istr let compare = compare end)

type cache_data =
  | Lastname
  | Firstname
  | Place
  | Source
  | Occupation

(** Create cache files  used by autocomplete *)
let create_cache base mode =
  let add acc x = if not (Gwdb.is_empty_string x) then IstrSet.add x acc else acc in
  let cache =
    match mode with
    | Lastname ->
      Gwdb.Collection.fold
        (fun acc p -> add acc (Gwdb.get_surname p) )
        IstrSet.empty
        (Gwdb.persons base)
    | Firstname ->
      Gwdb.Collection.fold
        (fun acc p -> add acc (Gwdb.get_first_name p) )
        IstrSet.empty (Gwdb.persons base)
    | Place ->
      let acc =
        Gwdb.Collection.fold
          (fun acc p ->
             List.fold_left
               (fun acc e -> add acc (Gwdb.get_pevent_place e)) acc (Gwdb.get_pevents p) )
          IstrSet.empty (Gwdb.persons base)
      in
      Gwdb.Collection.fold
        (fun acc f -> List.fold_left (fun acc e -> add acc (Gwdb.get_fevent_place e)) acc (Gwdb.get_fevents f) )
        acc (Gwdb.families base)
    | Source ->
      let acc =
        Gwdb.Collection.fold
          (fun acc p ->
             let acc = add acc (Gwdb.get_psources p) in
             List.fold_left (fun acc e -> add acc (Gwdb.get_pevent_src e)) acc (Gwdb.get_pevents p) )
          IstrSet.empty
          (Gwdb.persons base)
      in
      Gwdb.Collection.fold
        (fun acc f ->
           let acc = add acc (Gwdb.get_fsources f) in
           List.fold_left (fun acc e -> add acc (Gwdb.get_fevent_src e)) acc (Gwdb.get_fevents f) )
        acc
        (Gwdb.families base)
    | Occupation ->
       Gwdb.Collection.fold
         (fun occupations person ->
           add occupations (Gwdb.get_occupation person))
         IstrSet.empty (Gwdb.persons base)
  in
  let cache = List.rev_map (Gwdb.sou base) (IstrSet.elements cache) in
  List.sort
    (match mode with
     | Place -> Geneweb.Place.compare_places
     | Firstname | Lastname | Source | Occupation ->
       Utf8.alphabetic_order)
    cache

let cache_file_of_cache_data base_file = function
  | Lastname -> Filename.concat base_file "cache_surname"
  | Firstname -> Filename.concat base_file "cache_first_name"
  | Place -> Filename.concat base_file "cache_place"
  | Source -> Filename.concat base_file "cache_src"
  | Occupation -> Filename.concat base_file "cache_occupation"

let write_cache base cache_data =
  let cache = create_cache base cache_data in
  let base_dir = Geneweb.Util.bpath (Gwdb.bname base ^ ".gwb") in
  let cache_file = cache_file_of_cache_data base_dir cache_data in
  let oc = Secure.open_out_bin cache_file in
  Marshal.to_channel oc cache [ Marshal.No_sharing ] ;
  close_out oc
