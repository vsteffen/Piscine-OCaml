let ft_test_sign n =
	if n < 0
	then print_endline "negative"
	else print_endline "positive"

let main () =
	ft_test_sign 42 ;
	ft_test_sign 0 ;
	ft_test_sign (-42)

let () = main ()
