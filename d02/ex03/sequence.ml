let sequence n =
	if n < 0 then ""
	else let rec get_str output = function
		| [] -> output
		| head::tail -> output ^ (string_of_int head) ^ (get_str output tail)
	in
	let rec count_num count num = function
		| [] -> [count; num]
		| head::tail when head = num -> count_num (succ count) head tail
		| head::tail -> count::num::(count_num 1 head tail)
	in
	let rec loop n lst =
		if n == 0 then
			get_str "" lst
		else match lst with
			| [] -> ""
			| head::tail -> loop (pred n) (count_num 1 head tail)
	in
	loop n [1]

let main () =
		print_endline (sequence (-42)) ;
		print_endline (sequence 0) ;
		print_endline (sequence 1) ;
		print_endline (sequence 2) ;
		print_endline (sequence 3) ;
		print_endline (sequence 4) ;
		print_endline (sequence 5) ;
		print_endline (sequence 6)

let () = main ()