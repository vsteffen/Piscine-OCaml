let main () =
	List.iter (function color -> print_endline (Color.toString color)) Color.all ;
	List.iter (function color -> print_endline (Color.toStringVerbose color)) Color.all

let () = main ()
