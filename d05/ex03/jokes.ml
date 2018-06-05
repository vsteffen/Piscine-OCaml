let get_jokes filename =
	let array_of_jokes = ref [||] in
	let is_valid_joke joke = (String.length (String.trim !joke) > 0 ) in
	let joke = ref "" in
    begin try
		let ic = open_in filename in
        while true; do
            let line = input_line ic in
			begin
				if line = ";;" then
				begin
					if is_valid_joke joke then
						array_of_jokes :=  Array.append !array_of_jokes [|(!joke)|] ;
					joke := ""
				end
				else
					joke := !joke ^ line ^ "\n"
			end
        done;
        close_in ic ;
		print_endline !joke
        with
        | Sys_error err -> print_endline "Impossible to read file of joke(s)" ; exit 0
        | End_of_file -> ()
    end ;
	if is_valid_joke joke then
		array_of_jokes :=  Array.append !array_of_jokes [|(!joke)|] ;

	if Array.length !array_of_jokes > 0 then
		begin
			Random.self_init ();
			print_string (Array.get !array_of_jokes (Random.int (Array.length !array_of_jokes)))
		end
		else
			print_endline "Format or empty list of jokes"


let () =
	match (Array.length Sys.argv) with
	| 2 -> get_jokes (Array.get Sys.argv 1)
    | _ -> print_endline "Usage: jokes [FILENAME]"
