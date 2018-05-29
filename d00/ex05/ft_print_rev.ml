let ft_print_rev str =
	let rec loop index =
		if index >= 0 then
		begin
			print_char ( String.get str index ) ;
			loop (index - 1)
		end
	in
	loop (( String.length str ) - 1);
	print_char '\n'

let main () =
	ft_print_rev "Hello world !" ;
	ft_print_rev "0123456789" ;
	ft_print_rev ""

let () = main ()
