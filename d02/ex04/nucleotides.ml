type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
	phosphate : phosphate ;
	deoxyribose : deoxyribose ;
	nucleobase : nucleobase
}

let get_nucleobase_of_char = function
	| 'A' -> A
	| 'T' -> T
	| 'C' -> C
	| 'G' -> G
	| _ -> None

let generate_nucleotide base = {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = (get_nucleobase_of_char base)}

let nucleobase_to_str = function
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| None -> "None"

let print_nucleotide = function
	| {phosphate: _ ; deoxyribose: _; nucleobase: _} -> print_endline ("phosphate = " ^ phosphate) ; print_endline ("deoxyribose = " ^ deoxyribose) ; print_endline ("nucleobase = " ^ (nucleobase_to_str nucleobase))

let main () =
	let a = generate_nucleotide 'A' in
	let t = generate_nucleotide 'T' in
	let c = generate_nucleotide 'C' in
	let g = generate_nucleotide 'G' in
	let none = generate_nucleotide 'N' in

	print_nucleotide a ; print_char '\n' ;
	print_nucleotide t ; print_char '\n' ;
	print_nucleotide c ; print_char '\n' ;
	print_nucleotide g ; print_char '\n' ;
	print_nucleotide none ; print_char '\n'

let () = main ()