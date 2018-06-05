let eu_dist array1 array2 =
	let distance = ref 0. in
	for i = 0 to (Array.length array1 - 1) do
		distance := !distance +. (array1.(i) -. array2.(i)) ** 2.
	done;
	sqrt !distance

let () =
	print_float (eu_dist [|2.51 ; 1.49|] [|3.40 ; 4.42|]) ; print_char '\n' ;
	print_float (eu_dist [|2.51 ; -1.49|] [|-3.40 ; -4.42|]) ; print_char '\n' ;
	print_float (eu_dist [|2.1467 ; 4.8031 ; 0.4262|] [|0.7487 ; 1.1904 ; 3.9095|]) ; print_char '\n' ;
	print_float (eu_dist [||] [||]) ; print_char '\n'

(* https://calculator.vhex.net/post/calculator-result/euclidean-distance *)
