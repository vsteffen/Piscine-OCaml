class virtual atom name symbol atomic_number =
object (this)
	val _name = name
	val _symbol = symbol
	val _atomic_number = atomic_number

	method get_name = _name
	method get_symbol = _symbol
	method get_atomic_number = _atomic_number

	method set_name new_name = {< _name = new_name ; _symbol = _symbol ; _atomic_number = _atomic_number >}
	method set_symbol new_symbol = {< _name = _name ; _symbol = new_symbol ; _atomic_number = _atomic_number >}
	method set_atomic_number new_atomic_number = {< _name = _name ; _symbol = _symbol ; _atomic_number = new_atomic_number >}

	method to_string = ("Atom: " ^ (this#get_name) ^ ", " ^ (this#get_symbol) ^ ", " ^ (string_of_int (this#get_atomic_number)))
	method equals (that : atom) = (this#get_name = that#get_name && this#get_symbol = that#get_symbol && this#get_atomic_number = that#get_atomic_number)
end

class hydrogen =
object
	inherit atom "Hydrogen"	"H" 1
end

class carbon =
object
	inherit atom "Carbon"	"C" 6
end 

class oxygen =
object
	inherit atom "Oxygen"	"O" 8
end

class lithium =
object
	inherit atom "Lithium"	"Li" 3
end

class helium =
object
	inherit atom "Helium"	"He" 2
end

class iridium =
object
	inherit atom "Iridium"	"Ir" 77
end

class bismuth =
object
	inherit atom "Bismuth"	"Bi" 83
end

class neon =
object
	inherit atom "Neon"		"Ne" 10
end

class polonium =
object
	inherit atom "Polonium"	"Po" 84
end 
