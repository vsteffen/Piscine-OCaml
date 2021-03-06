 let ft_rot_n shift str =
	let shift_alphabet current =
		if current >= 'a' &&  current <= 'z' && ( int_of_char current ) + ( shift mod 26 ) > int_of_char 'z' then
			char_of_int (int_of_char 'a' - 1 +  (( int_of_char current ) + ( shift mod 26 )) - int_of_char 'z')
		else if current >= 'A' &&  current <= 'Z' && ( int_of_char current ) + ( shift mod 26 ) > int_of_char 'Z' then
			char_of_int (int_of_char 'A' - 1 +  (( int_of_char current ) + ( shift mod 26 )) - int_of_char 'Z')
		else if (current >= 'A' && current <= 'Z') || (current >= 'a' && current <= 'z') then
			char_of_int ((int_of_char current) + (shift mod 26))
		else
			current
	in
	String.map shift_alphabet str

let main () =
	print_endline( ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" ) ;
	print_endline( ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" ) ;
	print_endline( ft_rot_n 42 "0123456789" ) ;
	print_endline( ft_rot_n 2 "OI2EAS67B9" ) ;
	print_endline( ft_rot_n 0 "Damned !" ) ;
	print_endline( ft_rot_n 42 "" ) ;
	print_endline( ft_rot_n 1 "NBzlk qnbjr !" )

let () = main()
