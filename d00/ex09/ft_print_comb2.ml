let ft_print_comb2 () =
	let padding x =
		if x < 10 then
			print_char '0' ;
		print_int x
	in
	let rec loop left right =
		padding left ;
		print_string " " ;
		padding right ;

		if not (left == 98 && right == 99) then
			print_string ", " ;
		if right < 99 then
			loop left (right + 1)
		else if left < 98 then
			loop (left + 1) (left + 2)
	in
	loop 0 1 ;
	print_char '\n'

let main () =
	ft_print_comb2 ()

let () = main ()