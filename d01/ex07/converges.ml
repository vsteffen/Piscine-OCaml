let rec converges fn x n =
	if n < 0
		then false
	else if ( fn x ) = x
		then true
	else
		converges fn (fn x) (n - 1)


let main () =
	print_endline ( string_of_bool ( converges (( * ) 2) 2 5 ) ) ;
	print_endline ( string_of_bool ( converges (fun x -> x / 2) 2 3 ) ) ;
	print_endline ( string_of_bool ( converges (fun x -> x / 2) 2 2 ) )

let () = main ()
