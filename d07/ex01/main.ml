let main () =
	let people = new People.people "The Black Guardian" in
	let doctor0 = new Doctor.doctor "Number 11" 42 people in

	print_endline doctor0#to_string ;

	let doctor1 = doctor0#travel_in_time 2018 2019 in
	print_endline doctor1#talk ;
	print_endline doctor1#use_sonic_screwdriver ;

	print_endline doctor1#to_string ;
	let doctor2 = doctor1#weird_subject_take_dmg 60 in
	print_endline doctor2#to_string ;
	let doctor3 = doctor2#weird_subject_take_dmg 60 in
	print_endline doctor3#to_string ;

	ignore (doctor1#travel_in_time 2018 1970)


let () = main ()
