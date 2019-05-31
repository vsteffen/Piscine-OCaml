let main () =
	let hydrogen = new Atom.hydrogen in
	let oxygen = new Atom.oxygen in
	let carbon = new Atom.carbon in
	let carbon2 = new Atom.carbon in
	let nitrogen = new Atom.nitrogen in
	let neon = new Atom.neon in
	let silicon = new Atom.silicon in
	let chlorine = new Atom.chlorine in
	let iridium = new Atom.iridium in
	let bismuth = new Atom.bismuth in
	let polonium = new Atom.polonium in
	print_endline (hydrogen#to_string) ;
	print_endline (oxygen#to_string) ;
	print_endline (carbon#to_string) ;
	print_endline (nitrogen#to_string) ;
	print_endline (oxygen#to_string) ;
	print_endline (neon#to_string) ;
	print_endline (silicon#to_string) ;
	print_endline (chlorine#to_string) ;
	print_endline (iridium#to_string) ;
	print_endline (bismuth#to_string) ;
	print_endline (polonium#to_string) ;

	print_char '\n' ;

	print_endline (string_of_bool (carbon#equals hydrogen));
	print_endline (string_of_bool (carbon#equals carbon2))

let () = main ()
