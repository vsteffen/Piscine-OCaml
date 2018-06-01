let unrot42 str =
    String.map ( Cipher.rot_n (-42) ) str

let uncaesar str n =
    String.map ( Cipher.rot_n (-n) ) str

let xor str key =
	let calcXor c =
		((int_of_char c) lxor key) mod 255 in
	let fn_xor c =
		if calcXor c < 0 then
			char_of_int ((calcXor c) + 255)
		else
			char_of_int (calcXor c)
	in
	String.map ( fn_xor ) str

let rec ft_uncrypt (str:string) fn =
    match fn with
    | [] -> str
    | head::tail -> ft_uncrypt (head str) tail

let main () =
	print_endline ( "Caesar / uncaesar:" ) ;
	print_endline ( Cipher.caesar "abc" 1 ) ;
	print_endline ( uncaesar (Cipher.caesar "abc" 1) 1 ) ;
	print_endline ( Cipher.caesar "0123456789" 10 ) ;
	print_endline ( uncaesar (Cipher.caesar "0123456789" 10 ) 10) ;
	print_endline ( Cipher.caesar "0123456789" 296 ) ;
	print_endline ( uncaesar (Cipher.caesar "0123456789" 296 ) 296) ;

	print_char '\n';

	print_endline ( "rot42 / unrot42:" ) ;
	print_endline ( Cipher.rot42 "0123") ;
	print_endline ( unrot42 ( Cipher.rot42 "0123")) ;

	print_char '\n';

	print_endline ( "Xor:" ) ;
	print_endline ( Cipher.xor "a" 1 ) ;
	print_endline ( xor ( Cipher.xor "a" 1 ) 1) ;
	print_endline ( Cipher.xor "a" 32 ) ;
	print_endline ( xor ( Cipher.xor "a" 32 ) 32) ;

	print_char '\n' ;

	print_endline ( "ft_crypt / ft_uncrypt:" ) ;
	print_endline ( Cipher.ft_crypt "0123" [(fun str -> Cipher.caesar str 213) ; Cipher.rot42] )


let () = main ()
