let sum (x:float) (y:float) =
    x +. y

let () =
	print_string "Sum: 21.21 + 21.21 = ";
	print_float (sum 10. 11.); print_char '\n';

	print_string "Sum: 42. + (-84.) = ";
	print_float (sum 42. (-84.));  print_char '\n';

	print_string "Sum: 687651.4564 + (-687651.4564) = ";
	print_float (sum 687651.4564 (-687651.4564));  print_char '\n';
