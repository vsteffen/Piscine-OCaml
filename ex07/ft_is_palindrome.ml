let ft_is_palindrome str =
	let rec loop begIndex endIndex =
		if begIndex >= endIndex then
			true
		else if (String.get str begIndex) <> (String.get str endIndex) then
			false
		else
			loop ( begIndex + 1 ) ( endIndex - 1 )
	in
	loop 0 ((String.length str) - 1)


let main () =
	print_endline (string_of_bool (ft_is_palindrome "radar")) ;
	print_endline (string_of_bool (ft_is_palindrome "madam")) ;
	print_endline (string_of_bool (ft_is_palindrome "car")) ;
	print_endline (string_of_bool (ft_is_palindrome "")) ;
	print_endline (string_of_bool (ft_is_palindrome "eluparcettecrapule"))



let () = main ()
