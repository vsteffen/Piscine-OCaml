let main () =
	let methane = new Alkanes.methane in
	let ethane = new Alkanes.ethane in
	let octane = new Alkanes.octane in

	print_endline methane#to_string ;
	print_endline ethane#to_string ;
	print_endline octane#to_string ;

	print_char '\n' ;

	print_endline (string_of_bool (methane#equals ethane));
	print_endline (string_of_bool (methane#equals methane))

let () = main ()
