let example_of_files filename =
	let output = ref [] in
	let output_float = ref [||] in
	begin try
		let ic = open_in filename in
		while true; do
			let line = input_line ic in
			let get_split line = String.split_on_char ',' line in
			let length_split line_splited = (List.length line_splited) in
			let all_float_of_line line_splited length_float =
				ignore (List.mapi (fun i current -> if i > length_float then () else output_float := Array.append !output_float [|(float_of_string current)|]) line_splited) ;
				!output_float
			in
			output_float := [||] ;
			output :=  !output @ [(all_float_of_line (get_split line) ((length_split (get_split line)) - 2)) , (List.nth (get_split line) ((length_split (get_split line)) - 1))]
		done ;
	with
	| Sys_error err -> print_endline "Impossible to read file" ; exit 0
	| End_of_file -> ()
	end;
	!output

let () =
	let print_array_and_string array_of_float str =
		print_string "[|" ;
		Array.iteri (fun i fl -> if i > 0 then print_string " ; " ; print_float fl) array_of_float ;
		print_string "|], \"" ;
		print_string str ;
		print_string "\")\n"
	in
	List.iter (fun (array_of_float, str) -> print_array_and_string array_of_float str) (example_of_files "ionosphere.test.csv")
(* ([[|1.0; 0.5 ;0.3 |], "g"]) *)
