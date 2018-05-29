let ft_string_all func str =
	let rec loop index =
		if index >= 0 then
		begin
			if func ( String.get str index ) then
				loop (index - 1)
			else
				false
		end
		else true
	in
	loop ((String.length str) - 1)

let is_digit c = c >= '0' && c <= '9'

let main () =
	print_endline (string_of_bool (ft_string_all is_digit "0123456789")) ;
	print_endline (string_of_bool (ft_string_all is_digit "O12EAS67B9"))


let () = main ()
