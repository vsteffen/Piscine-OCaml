let main () =
	let getQueen = List.nth Value.all 10 in
	(* let getT2 = List.hd Value.all in *)
	(* let getAS = List.nth Value.all 12 in *)

	print_endline ( "Get card int value:" ) ;
	List.iter (function value -> print_int (Value.toInt value) ; print_char '\n') Value.all ;

	print_endline ( "\nValue.toString:" ) ;
	List.iter (function value -> print_endline (Value.toString value)) Value.all ;

	print_endline ( "\nValue.toStringVerbose:" ) ;
	List.iter (function value -> print_endline (Value.toStringVerbose value)) Value.all ;

	print_endline ( "\nValue.next:" ) ;
	print_endline (Value.toStringVerbose (Value.next (getQueen))) ;
	(* print_endline (Value.toStringVerbose (Value.next (getAS))) ; *)

	print_endline ( "\nValue.previous:" ) ;
	(* print_endline (Value.toStringVerbose (Value.previous (getT2))) ; *)
	print_endline (Value.toStringVerbose (Value.previous (getQueen)))

let () = main ()
