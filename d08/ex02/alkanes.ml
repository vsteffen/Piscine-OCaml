let get_alkane_atom_lst n = ((new Atom.carbon)#create_list n) @ ((new Atom.hydrogen)#create_list (n * 2 + 2))

let get_alkane_name = function
		| 1 -> "Methane"
		| 2 -> "Ethane"
		| 3 -> "Propane"
		| 4 -> "Butane"
		| 5 -> "Pentane"
		| 6 -> "Hexane"
		| 7 -> "Heptane"
		| 8 -> "Octane"
		| 9	-> "Nonane"
		| 10 -> "Decane"
		| 11 -> "Undecane"
		| 12 -> "Dodecane"
		| _ -> "Unknownane"

class virtual alkanes n =
object (this)
	inherit Molecules.molecules (get_alkane_name n) (get_alkane_atom_lst n)
	method to_string = ("Molecule: " ^ (this#get_name) ^ ", " ^ (this#get_formula) ^ " (Alkane family)" )
end

class methane =
object
	inherit alkanes 1
end

class ethane =
object
	inherit alkanes 2
end

class propane =
object
	inherit alkanes 3
end

class butane =
object
	inherit alkanes 4
end

class pentane =
object
	inherit alkanes 5
end

class hexane =
object
	inherit alkanes 6
end

class heptane =
object
	inherit alkanes 7
end

class octane =
object
	inherit alkanes 8
end

class nonane =
object
	inherit alkanes 9
end

class decane =
object
	inherit alkanes 10
end

class undecane =
object
	inherit alkanes 11
end

class dodecane =
object
	inherit alkanes 12
end