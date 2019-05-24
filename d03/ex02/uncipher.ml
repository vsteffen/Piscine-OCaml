let unrot42 str =
	String.map (fun c -> char_of_int (
		if (((int_of_char c) - 42) mod 255) < 0 then
			(((int_of_char c) - 42) mod 255) + 255
		else
		 	(((int_of_char c) - 42) mod 255))) str

let uncaesar n str =
	String.map (fun c -> char_of_int (
		if (((int_of_char c) - n) mod 255) < 0 then
			(((int_of_char c) - n) mod 255) + 255
		else
		 	(((int_of_char c) - n) mod 255))) str

let rec ft_uncrypt (str:string) (lst:(string->string) list) =
	match lst with
	| head::tail -> ft_uncrypt (head str) tail
	| [] -> str


let main () =
	let before = "Before -> " in
	let after  = "After  -> " in

	let caesar_str1 = "abc" in
	let caesar_str2 = "0123456789" in
	let caesar_str3 = "012345678901234567890123456789012345678901" in
	print_endline ( "Caesar / uncaesar:" ) ;
	print_endline (before ^ ( uncaesar 1 (Cipher.caesar 1 caesar_str1) ) );
	print_endline (after  ^ ( Cipher.caesar 1 caesar_str1 ) );
	print_endline (before ^ ( uncaesar (-3) (Cipher.caesar (-3) caesar_str2 ) ) );
	print_endline (after  ^ ( Cipher.caesar (-3) caesar_str2 ) );
	print_endline (before ^ ( uncaesar 42 (Cipher.caesar 42 caesar_str3 ) ) );
	print_endline (after  ^ ( Cipher.caesar 42 caesar_str3 ) );

	print_char '\n';

	let rot42_str1 = "0123456789" in
	let rot42_str2 = caesar_str3 in
	print_endline ( "rot42 / unrot42:" ) ;
	print_endline (before ^ ( unrot42 ( Cipher.rot42 rot42_str1)) );
	print_endline (after  ^ ( Cipher.rot42 rot42_str1) );
	print_endline (before ^ ( unrot42 ( Cipher.rot42 rot42_str2)) );
	print_endline (after  ^ ( Cipher.rot42 rot42_str2) );

	print_char '\n';

	let xor_str = "SuperPoulet" in
	print_endline ( "Xor:" ) ;
	print_endline (before ^ ( Cipher.xor 1 ( Cipher.xor 1 xor_str ) ) );
	print_endline (after  ^ ( Cipher.xor 1 xor_str ) );
	print_endline (before ^ ( Cipher.xor 42 ( Cipher.xor 42 xor_str ) ) );
	print_endline (after  ^ ( Cipher.xor 42 xor_str ) );

	print_char '\n' ;
	let crypted =  Cipher.ft_crypt caesar_str3 [Cipher.rot42 ; Cipher.caesar 1] in
	print_endline ( "ft_crypt / ft_uncrypt:" ) ;
	print_endline ( before ^ (ft_uncrypt crypted [unrot42 ; uncaesar 1]) ) ;
	print_endline ( after  ^ crypted )

let () = main ()
