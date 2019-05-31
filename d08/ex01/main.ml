let main () =
	let tnt = new Molecules.trinitrotoluene in
	let water = new Molecules.water in
	let carbon_dioxyde = new Molecules.carbon_dioxyde in
	let trinitrotoluene = new Molecules.trinitrotoluene in
	let trichloroisocyanuric_acid = new Molecules.trichloroisocyanuric_acid in
	let cycloserine = new Molecules.cycloserine in
	let glutamine_tritbdms = new Molecules.glutamine_tritbdms in

	print_endline tnt#to_string ;
	print_endline water#to_string ;
	print_endline carbon_dioxyde#to_string ;
	print_endline trinitrotoluene#to_string ;
	print_endline trichloroisocyanuric_acid#to_string ;
	print_endline cycloserine#to_string ;
	print_endline glutamine_tritbdms#to_string ;

	print_char '\n' ;

	print_endline (string_of_bool (tnt#equals water));
	print_endline (string_of_bool (tnt#equals tnt))

let () = main ()
