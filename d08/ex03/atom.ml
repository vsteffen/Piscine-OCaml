class virtual atom name symbol atomic_number =
object (this)
	method get_name = name
	method get_symbol = symbol
	method get_atomic_number = atomic_number

	method create_list repeat =
		let rec loop repeat =
			if repeat > 0 then
				Oo.copy this :: (loop (pred repeat))
			else
				[]
		in
		loop repeat

	method to_string = ("Atom: " ^ (this#get_name) ^ ", " ^ (this#get_symbol) ^ ", " ^ (string_of_int (this#get_atomic_number)))
	method equals (that : atom) = (this#get_name = that#get_name && this#get_symbol = that#get_symbol && this#get_atomic_number = that#get_atomic_number)
end

class hydrogen =
object
	inherit atom "Hydrogen"	"H" 1
end

class helium =
object
	inherit atom "Helium"	"He" 2
end

class lithium =
object
	inherit atom "Lithium"	"Li" 3
end

class carbon =
object
	inherit atom "Carbon"	"C" 6
end 

class nitrogen =
object
	inherit atom "Nitrogen"	"N" 7
end

class oxygen =
object
	inherit atom "Oxygen"	"O" 8
end

class neon =
object
	inherit atom "Neon"		"Ne" 10
end

class silicon =
object
	inherit atom "Silicon"	"Si" 14
end

class chlorine =
object
	inherit atom "Chlorine"	"Cl" 17
end

class iridium =
object
	inherit atom "Iridium"	"Ir" 77
end

class bismuth =
object
	inherit atom "Bismuth"	"Bi" 83
end

class polonium =
object
	inherit atom "Polonium"	"Po" 84
end 
