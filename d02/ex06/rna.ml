type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None

type nucleotide = {
	phosphate : phosphate ;
	deoxyribose : deoxyribose ;
	nucleobase : nucleobase
}

type helix = nucleotide list

type rna = nucleobase list

let get_nucleobase_of_char = function
	| 'A' -> A
	| 'T' -> T
	| 'C' -> C
	| 'G' -> G
	| 'U' -> U
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
		| 4 -> 'U'
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
	| U -> "U"
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

let get_nucleobase_in_helix nucleotide =
	match nucleotide with
	| {phosphate: _ ; deoxyribose: _; nucleobase: _} -> nucleobase

let complementary_helix (old : helix) =
	let corresponding_base base =
		match base with
		| A -> 'T'
		| T -> 'A'
		| C -> 'G'
		| G -> 'C'
		| U -> 'U'
		| None -> 'Q'
	in
	let rec iter_old_helix old new_h = 
		match old with
		| [] -> new_h
		| head::tail -> iter_old_helix tail (new_h @ [(generate_nucleotide (corresponding_base (get_nucleobase_in_helix head)))])
	in
	(iter_old_helix old [] : helix)

let generate_rna (h : helix) =
	let corresponding_base_rna base =
		match base with
		| A -> U
		| T -> A
		| C -> G
		| G -> C
		| U -> U
		| None -> None
	in
	let rec iter_helix h output =
		match h with
		| [] -> output
		| head::tail -> iter_helix tail (output @ [corresponding_base_rna (get_nucleobase_in_helix head)])
	in
	(iter_helix h [] : rna)

let rec print_rna = function
	| [] -> ()
	| head::tail -> print_string (nucleobase_to_string head) ; print_rna tail

let main () =
	Random.self_init() ;
	let h1 = generate_helix 5 in	
	let rna = generate_rna h1 in	
	print_endline (helix_to_string h1) ;
	print_rna rna ; print_char '\n'

let () = main ()