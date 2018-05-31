let crossover list1 list2 =
	let rec elem_exist list_to_iter to_found =
		match list_to_iter with
		| [] -> false
		| head::tail -> (to_found = head) || (elem_exist tail to_found)
	in
 	let rec iter_lists pos_list1 output=
		match pos_list1 with
		| [] -> output
		| head::tail
			->	if (elem_exist output head) = false && (elem_exist list2 head) = true then
					iter_lists tail (output @ [head])
				else
					iter_lists tail output

	in
	if list1 = [] || list2 = [] then
		[]
	else
		iter_lists list1 []

let main () =
	let rec iter_example liste =
		match liste with
		| [] -> ()
		| tete::queue -> print_int tete ; print_char ' ' ; iter_example queue
	in
	iter_example ( crossover [1; 2; 3;] [1; 2; 3;] ) ; print_char '\n' ;
	iter_example ( crossover [1; 2; 3;] [3; 3; 3;] ) ; print_char '\n' ;
	iter_example ( crossover [1; 2; 3;] [4; 5; 6;] ) ; print_char '\n' ;
	iter_example ( crossover [] [] ) ; print_char '\n'

let () = main ()
