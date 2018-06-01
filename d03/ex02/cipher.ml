let rot_n n c =
	let calcRot = ((int_of_char c) + n ) mod 255 in
	if calcRot < 0 then
		char_of_int (calcRot + 255)
	else
		char_of_int calcRot

let rot42 str =
	String.map ( rot_n 42 ) str

let caesar str n =
    String.map ( rot_n n ) str

let xor str key =
	let fn_xor c = char_of_int ((int_of_char c) lxor key) in
	String.map fn_xor str

let rec ft_crypt (str:string) fn =
    match fn with
    | [] -> str
    | head::tail -> ft_crypt (head str) tail
