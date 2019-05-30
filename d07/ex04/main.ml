let main () =
	let armyDalek0 = new Army.army [] in
	let armyDalek1 = armyDalek0#add (new Dalek.dalek "Dalek") in
	let armyDalek2 = armyDalek1#add (new Dalek.dalek "Dalek") in
	let armyDalek3 = armyDalek2#add (new Dalek.dalek "Dalek") in
	let doctor name = new Doctor.doctor name in
	let sidekick = new People.people "Sidekick" in
	let armyDoctor0 = new Army.army [] in
	let armyDoctor1 = armyDoctor0#add (doctor "Number 6" 42 sidekick) in
	let armyDoctor2 = armyDoctor1#add (doctor "Number 4" 18 sidekick) in
	let armyDoctor3 = armyDoctor2#add (doctor "Number 8" 30 sidekick) in
	let people name = new People.people name in
	let armyPeople0 = new Army.army [] in
	let armyPeople1 = armyPeople0#add (people "The Master") in
	let armyPeople2 = armyPeople1#add (people "Silurians") in
	let armyPeople3 = armyPeople2#add (people "The Black Gardian") in

	print_char '\n' ;

	let galifrey = new Galifrey.galifrey (armyDalek3#get_army) (armyDoctor3#get_army) (armyPeople3#get_army) in
	galifrey#do_time_war

let () = main ()