let gray n =
	let rec print_code x n =
		if n > 0 then
			print_code (x / 2) (pred n); print_int (x mod 2)
	in
	let gray_encode n =
		n lxor (n lsr 1)
	in
	let rec loop_gray i =
		if i < 1 lsl n
		then
		(
			if i > 0 then
				print_string " " ;
			print_code (gray_encode i) (pred n);
			loop_gray (succ i)
		)
	in
	if n > 0 then loop_gray 0;
	print_char '\n'

let main () =
	gray 1 ;
	gray 2 ;
	gray 3

let () = main ()