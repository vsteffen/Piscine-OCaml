let fibonacci n =
	let rec fibonacci_tail_rec i prev1 prev2 =
		if i = n then
			prev1 + prev2
		else if i = 0 then
			fibonacci_tail_rec 1 1 0
		else if i = 1 then
			fibonacci_tail_rec 2 1 0
		else
			fibonacci_tail_rec (i + 1) (prev1 + prev2) prev1
	in
	if n < 0
		then (-1)
	else
		fibonacci_tail_rec 0 0 0
(* 0 1 2 3 4 5 6 *)
(* 0 1 1 2 3 5 8 *)

let main () =
	print_int ( fibonacci (-42) ) ; print_char '\n' ;
	print_int ( fibonacci 1 ) ; print_char '\n' ;
	print_int ( fibonacci 3 ) ; print_char '\n' ;
	print_int ( fibonacci 6 ) ; print_char '\n'

let () = main ()
