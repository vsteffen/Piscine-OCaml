type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
	phosphate : phosphate ;
	deoxyribose : deoxyribose ;
	nucleobase : nucleobase
}

type helix = nucleotide list

let get_nucleobase_of_char = function
	| 'A' -> A
	| 'T' -> T
	| 'C' -> C
	| 'G' -> G
	| _ -> None

let generate_nucleotide base = {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = (get_nucleobase_of_char base)}

let generate_helix n =
	let get_random_nucleobase () =
		let rand = Random.int 4 in
		match rand with
		| 0 -> 'A'
		| 1 -> 'T'
		| 2 -> 'C'
		| 3 -> 'G'
		| _ -> 'Q'
	in
	let rec add_nucleotide n lst =
		if n == 0 then lst
		else add_nucleotide (pred n) ((generate_nucleotide (get_random_nucleobase ())) :: lst)
	in
	if n < 1 then []
	else (add_nucleotide n [] : helix)


let nucleobase_to_string = function
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| None -> "None"

let print_nucleotide = function
	| {phosphate: _ ; deoxyribose: _; nucleobase: _} -> print_endline ("phosphate = " ^ phosphate) ; print_endline ("deoxyribose = " ^ deoxyribose) ; print_endline ("nucleobase = " ^ (nucleobase_to_string nucleobase))

let helix_to_string (h : helix) =
	let rec iter_helix h output = 
		match h with
		| [] -> output
		| head::tail ->
		(
			match head with 
			| {phosphate: _ ; deoxyribose: _; nucleobase: _} -> iter_helix tail (output ^ (nucleobase_to_string nucleobase))
		)
	in
	iter_helix h ""

let complementary_helix (old : helix) =
	let corresponding_base base =
		match base with
		| A -> 'T'
		| T -> 'A'
		| C -> 'G'
		| G -> 'C'
		| None -> 'Q'
	in
	let get_nucleobase_in_helix nucleotide =
		match nucleotide with
		| {phosphate: _ ; deoxyribose: _; nucleobase: _} -> nucleobase
	in
	let rec iter_old_helix old new_h = 
		match old with
		| [] -> new_h
		| head::tail -> iter_old_helix tail (new_h @ [(generate_nucleotide (corresponding_base (get_nucleobase_in_helix head)))])
	in
	(iter_old_helix old [] : helix)

let main () =
	Random.self_init() ;
	let h1 = generate_helix 3 in
	let h2 = complementary_helix h1 in
	let h3 = generate_helix (-42) in
	let h4 = generate_helix 10 in
	print_endline (helix_to_string h1) ;
	print_endline (helix_to_string h2) ;
	print_char '\n' ;
	print_endline (helix_to_string h3) ;
	print_endline (helix_to_string h4)

let () = main ()