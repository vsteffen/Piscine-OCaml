let repeat_x n =
	if n < 0
	then "Error"
	else
	begin
		let rec loop n output =
			if n > 0 then
				loop ( n - 1 ) output ^ "x"
			else
				output
		in
		loop n ""
	end

let main () =
	print_endline ( repeat_x (-1) ) ;
	print_endline ( repeat_x 0 ) ;
	print_endline ( repeat_x 1 ) ;
	print_endline ( repeat_x 3 )

let () = main ()
