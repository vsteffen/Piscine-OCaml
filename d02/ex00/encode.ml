let encode input =
	let rec find_occurrence toFound acc =
		match input with
		| [] -> acc
		| tete::queue -> if find_occurrence queue (acc + 1)
	in
	let rec iter_list input ouput =
		match input with
		| [] -> []
		| tete::queue
			->	if (find_occurrence tete 0) > 0 then
					output @ [tete] ;
					iter_list queue
	in
	iter_list input []

let main () =
	let rec print_list1 liste =
		match liste with
		| [] -> ()
		| tete::queue -> print_int ( tete ) ; print_list1 queue
	in
	print_list1 ([1; 2; 3])

let () = main ()
