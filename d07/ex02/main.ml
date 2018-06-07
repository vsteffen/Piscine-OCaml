let main () =
	let random = new People.people "The Black Guardian" in
	let doctor = new Doctor.doctor "Number 11" 42 random in
	let dalek = new Dalek.dalek "Dalek" in
	print_endline doctor#to_string ;
	print_endline dalek#to_string ;

	print_endline doctor#talk ;
	print_endline dalek#talk ;

	print_endline dalek#talk ;
	print_endline (dalek#exterminate random) ;
	print_endline random#die ;
	print_endline dalek#to_string ;

	print_endline doctor#use_sonic_screwdriver ;
	let dalekDie = dalek#take_dmg 9000 in
	print_endline dalekDie#die ;
	print_endline dalekDie#to_string


let () = main ()
