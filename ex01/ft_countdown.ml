let rec ft_countdown count =
	let print_of_count n =
		print_int n ;
		print_char '\n'
	in
	if count <= 0 then
		print_of_count 0
	else
		begin
			print_of_count count ;
			ft_countdown (count - 1)
		end

let main () =
	ft_countdown 0 ;
	print_char '\n' ;

	ft_countdown 5 ;
	print_char '\n' ;

	ft_countdown (-42)

let () = main ()
