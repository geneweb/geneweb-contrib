let main () =
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.fprintf stdout "%s\n" (Name.lower Sys.argv.(i))
  done

let _ = main ()
