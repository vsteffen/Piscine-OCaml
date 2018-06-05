type 'a ft_ref = {mutable to_ref : 'a}

let return value =
    let (mutable_record : 'a ft_ref) = {to_ref = value} in
	mutable_record

let get (mutable_record :'a ft_ref) =
    mutable_record.to_ref

let set (mutable_record : 'a ft_ref) value =
    mutable_record.to_ref <- value

let bind (mutable_record : 'a ft_ref) (fn : 'a -> 'b ft_ref) =
    fn mutable_record.to_ref

let main () =
	let reference = return "Cats are better than otters" in
	print_endline (get reference) ;

	let reference2 = return 21 in
	print_int (get reference2) ; print_char '\n' ;
	set reference2 42 ;
	print_int (get reference2) ; print_char '\n' ;

	let reference3 = bind reference2 ((fun x -> return (char_of_int (get reference2)))) in
	print_char (get reference3) ; print_char '\n'


let () = main ()
