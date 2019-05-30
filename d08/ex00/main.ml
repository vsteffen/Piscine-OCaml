let main () =
	let hydrogen = new Atom.hydrogen in
	let oxygen = new Atom.oxygen in
	let carbon = new Atom.carbon in
	let carbon2 = new Atom.carbon in
	print_endline (hydrogen#to_string) ;
	print_endline (oxygen#to_string) ;
	print_endline (carbon#to_string) ;

	print_char '\n' ;

	print_endline (string_of_bool (carbon#equals hydrogen));
	print_endline (string_of_bool (carbon#equals carbon2));

	print_char '\n' ;

	let oxygen_mod = oxygen#set_atomic_number 999 in
	print_endline (oxygen_mod#to_string)

let () = main ()
