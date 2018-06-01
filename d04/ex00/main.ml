let main () =
	List.iter (function a -> print_endline (Color.toString a)) Color.all ;
	List.iter (function a -> print_endline (Color.toStringVerbose a)) Color.all

let () = main ()
