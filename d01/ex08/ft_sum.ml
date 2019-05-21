let ft_sum fn iLower iUpper =
	let rec loop i acc =
		if i > iUpper then
			acc
		else
			loop ( i + 1 ) (acc +. (fn i))
	in
	if iLower > iUpper
		then nan
	else
		loop iLower 0.


let main () =
	print_float ( ft_sum (fun i -> float_of_int (i * i)) 1 10 ) ; print_char '\n' ;
	print_float ( ft_sum (fun i -> float_of_int (i * i)) 0 1 ) ; print_char '\n' ;
	print_float ( ft_sum (fun i -> float_of_int (i * i)) 0 2 ) ; print_char '\n' ;
	print_float ( ft_sum (fun i -> float_of_int (i * i)) 1 0 ) ; print_char '\n' ;
	print_float ( ft_sum (fun i -> float_of_int (i * i)) (-1) 2 ) ; print_char '\n' ;
	print_float ( ft_sum (fun i -> float_of_int (i * i)) 1 100000000 ) ; print_char '\n'

let () = main ()
