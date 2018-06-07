let main () =

	print_endline "Create army of dalek" ;
	let armyDalek0 = new Army.army [] in
	print_endline ("Army of dalek: " ^ (string_of_int (List.length armyDalek0#get_army)) ^ " daleks") ;
	let armyDalek1 = armyDalek0#add (new Dalek.dalek "Dalek") in
	let armyDalek2 = armyDalek1#add (new Dalek.dalek "Dalek") in
	let armyDalek3 = armyDalek2#add (new Dalek.dalek "Dalek") in
	print_endline ("Army of dalek: " ^ (string_of_int (List.length armyDalek3#get_army)) ^ " daleks") ;
	List.iter (fun elem -> print_endline elem#to_string) armyDalek3#get_army ;
	let armyDalek2Del = armyDalek3#delete in
	print_endline ("Army of dalek: " ^ (string_of_int (List.length armyDalek2Del#get_army)) ^ " daleks") ;

	let doctor name = new Doctor.doctor name in
	let sidekick = new People.people "Sidekick" in
	print_endline "\nCreate army of doctor" ;
	let armyDoctor0 = new Army.army [] in
	print_endline ("Army of doctor: " ^ (string_of_int (List.length armyDoctor0#get_army)) ^ " doctor") ;
	let armyDoctor1 = armyDoctor0#add (doctor "Number 6" 42 sidekick) in
	let armyDoctor2 = armyDoctor1#add (doctor "Number 4" 18 sidekick) in
	let armyDoctor3 = armyDoctor2#add (doctor "Number 8" 30 sidekick) in
	print_endline ("Army of doctor: " ^ (string_of_int (List.length armyDoctor3#get_army)) ^ " doctor") ;
	List.iter (fun elem -> print_endline elem#to_string) armyDoctor3#get_army ;

	let people name = new People.people name in
	print_endline "\nCreate army of people" ;
	let armyPeople0 = new Army.army [] in
	print_endline ("Army of people: " ^ (string_of_int (List.length armyPeople0#get_army)) ^ " people") ;
	let armyPeople1 = armyPeople0#add (people "The Master") in
	let armyPeople2 = armyPeople1#add (people "Silurians") in
	let armyPeople3 = armyPeople2#add (people "The Black Gardian") in
	print_endline ("Army of people: " ^ (string_of_int (List.length armyPeople3#get_army)) ^ " people") ;
	List.iter (fun elem -> print_endline elem#to_string) armyPeople3#get_army


let () = main ()
