let repeat_string ?str:(str="x") n =
	if n < 0
	then "Error"
	else
	begin
		let rec loop n output =
			if n > 0 then
				loop ( n - 1 ) output ^ str
			else
				output
		in
		loop n ""
	end


let main () =
	print_endline ( repeat_string (-1) ) ;
	print_endline ( repeat_string 0 ) ;
	print_endline ( repeat_string ~str:"Toto" 1 ) ;
	print_endline ( repeat_string 2 ) ;
	print_endline ( repeat_string ~str:"a" 5 ) ;
	print_endline ( repeat_string ~str:"what" 3 )

let () = main ()
