let sequence n =
	let rec get_output lst output =
		match lst with
		| [] -> output
		| head::tail -> get_output tail (output ^ (string_of_int head)) 
	in
	let rec count_same_num lst num count =
		match lst with
		| head::tail when head == num -> count_same_num tail num (succ count)
		| _ -> count, num, tail
	in
	let rec get_seq_n lst new_seq =
		match lst with
		| [] -> new_seq
		| head::tail -> 
		(
			let (count, num, next) = count_same_num head tail 1 ;
			get_seq_n next (new_seq @ [count; num])
		)
	in
	let rec compute n lst =
		if n > 0 then
			compute (pred n) (get_seq_n lst [])
		else
			lst
	in
	get_output (compute n [1]) ""

let main () =
	print_string (sequence 1)

let () = main ()