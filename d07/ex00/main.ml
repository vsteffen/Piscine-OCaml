let main () =
	let people1 = new People.people "The Black Guardian" in
	print_endline people1#to_string ;
	print_endline people1#talk ;
	print_endline people1#die ;

	print_char '\n' ;

	let people2 = new People.people "The Ood" in
	print_endline people2#to_string ;
	print_endline people2#talk ;
	print_endline people2#die


let () = main ()
