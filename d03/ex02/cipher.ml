let rot42 str =
	String.map (fun c -> char_of_int (((int_of_char c) + 42) mod 255)) str

let caesar n str =
	String.map (fun c -> char_of_int (((int_of_char c) + n) mod 255)) str

let xor key str =
	let fn_xor c = char_of_int (((int_of_char c) lxor (key mod 255)) mod 255) in
	String.map fn_xor str

let rec ft_crypt (str:string) (lst:(string->string) list) =
	match lst with
	| head::tail -> ft_crypt (head str) tail
	| [] -> str