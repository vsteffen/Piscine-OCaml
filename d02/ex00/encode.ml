let encode input =
	let rec find_occurrence pos_list to_found acc =
		match pos_list with
		| [] -> acc
		| tete::queue -> if to_found = tete then
							find_occurrence queue to_found (acc + 1)
						else
							acc
	in
	let rec iter_list pos_list output n_occur =
		if n_occur == 1 then
		 	iter_list pos_list output 0
		else if n_occur > 1 then
			match pos_list with
			| [] -> []
			| tete::queue -> iter_list queue output (n_occur - 1)
		else
			match pos_list with
			| [] -> output
			| tete::queue -> iter_list queue (output @ [((find_occurrence queue tete 1), tete)]) (find_occurrence queue tete 1)
	in
	if input = [] then
		[]
	else
		iter_list input [] 0

let main () =
	let print_tuple (n_occur, elem) =
	 	print_int n_occur ; print_string (" , ") ; print_int elem ; print_char '\n'
	in
	let rec iter_example liste =
		match liste with
		| [] -> ()
		| tete::queue -> print_tuple tete ; iter_example queue
	in
	iter_example (encode [1; 2; 2; 2; 3 ; 3 ;]) ;
	iter_example (encode []) ;
	print_char '\n' ;
	iter_example (encode [42])

let () = main ()
