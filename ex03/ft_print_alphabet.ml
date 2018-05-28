let ft_print_alphabet () =
	let ascii_of_a = int_of_char 'a' in
	let ascii_of_z = int_of_char 'z' in
	let rec loop ascii_current_char =
		if ascii_current_char <= ascii_of_z then
		let char_of_current = char_of_int ascii_current_char in
			print_char char_of_current ;
			loop (ascii_current_char + 1)
	in
	loop ascii_of_a ;
	print_char '\n'

let main () =
 	ft_print_alphabet ()

let () = main ()
