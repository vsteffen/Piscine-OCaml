let rec ft_power n power =
	if power = 0
	then 1
	else n * (ft_power n (power - 1) )

let main () =
	print_int (ft_power 2 4) ; print_char '\n' ;
	print_int (ft_power 3 0) ; print_char '\n' ;
	print_int (ft_power 0 5) ; print_char '\n'

let () = main ()
