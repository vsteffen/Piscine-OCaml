type radar = float array * string

let eu_dist array1 array2 =
	let distance = ref 0. in
	for i = 0 to (Array.length array1 - 1) do
		distance := !distance +. (array1.(i) -. array2.(i)) ** 2.
	done;
	sqrt !distance

let one_nn (lst : radar list) (radar_to_find : radar) =
	let dist = ref infinity in
	let find_k_nearest (vectors1, class_char1) (vectors2, class_char2) =
		let eu = eu_dist (fst radar_to_find) vectors2 in
		if eu < !dist then
		begin
			dist := eu;
			(vectors2, class_char2)
		end
		else
			(vectors1, class_char1)
	in
	let rec iter_lst lst output =
		match lst with
		| [] -> output
		| head::tail -> iter_lst tail (find_k_nearest output head)
	in
	snd (iter_lst lst radar_to_find)

let main () =
	let training = [
		([| 1.; |], "first");
		([| 10.; |], "second");
		([| 100.; |], "third")]
	in
	let print_res radar_to_find training_list =
		print_endline ("Nearest neighbours of " ^ (snd radar_to_find) ^ " --> " ^ (one_nn training_list radar_to_find))
	in

	print_res ([| -3.; |], "A") training;
	print_res ([| 42.; |], "B") training;
	print_res ([| 9999.; |], "C") training;

	print_char '\n' ;

	let training2 = [
		([| 1.; 1.; 1.|], "first");
		([| 2.; 2.; 2.|], "second");
		([| 3.; 3.; 3.|], "third")]
	in
	print_res ([| 1.; -1.; 1.|], "D") training2;
	print_res ([| 2.; 3.; 2.|], "E") training2;
	print_res ([| 42.; 42.; 42.|], "F") training2


let () = main ()