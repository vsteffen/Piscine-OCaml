let ft_print_comb () =
	let number = 0 in
	let get_hundred number = number / 100 in
	let get_ten number = (number mod 100) / 10 in
	let get_numeral number = number mod 10 in
	let verif_appear_before hundred ten numeral =
		if hundred > ten && hundred > numeral
			then false
		else if ten > numeral || ten < hundred
			then false
		else true
	in
	let rec loop comb =
		if comb < 1000 then
			begin
				if ((get_hundred comb) <> (get_ten comb) && (get_hundred comb) <> (get_numeral comb) && (get_ten comb) <> (get_numeral comb))
					&& ( verif_appear_before (get_hundred comb) (get_ten comb) (get_numeral comb) )
				then
					begin
						if (comb <> 12) then
							print_string ", " ;
						if (comb < 100) then
							print_int 0 ;
						if (comb < 10 ) then
							print_int 0 ;
						print_int comb ;
					end;
				loop (comb + 1)
			end
	in
	loop number ;
	print_string "\n"

let main () =
	ft_print_comb ()

let () = main ()
