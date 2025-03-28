val change_only_old_access :
  old_access:Def.access -> new_access:Def.access -> string -> unit

val access_everybody : Def.access -> string -> unit

val most_recent_year_of : Gwdb.person -> int option

val oldest_year_of : Gwdb.person -> int option
