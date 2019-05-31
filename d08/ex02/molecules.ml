class virtual molecule name atom_lst =
object (this)
	method get_name = name
	method get_formula =
		begin
			let atom_sorted = List.sort (fun elem1 elem2 -> compare (elem1#get_symbol) (elem2#get_symbol)) atom_lst in
			let rec count_atom count output = function
				| [] -> output
				| head::middle::tail when head#get_symbol = middle#get_symbol -> count_atom (succ count) output (middle::tail)
				| head::middle::tail -> count_atom 1 (output ^ head#get_symbol ^ (if count > 1 then (string_of_int count) else "")) (middle::tail)	 		
				| head::tail -> output ^ head#get_symbol ^ (if count > 1 then (string_of_int count) else "")
			in
			count_atom 1 "" atom_sorted
		end

	method to_string = ("Molecule: " ^ (this#get_name) ^ ", " ^ (this#get_formula))
	method equals (that : molecule) = (this#get_name = that#get_name && this#get_formula = that#get_formula)
end


class trinitrotoluene =
object
	inherit molecule "Trinitrotoluene" (((new Atom.nitrogen)#create_list 3) @ ((new Atom.hydrogen)#create_list 5) @ ((new Atom.oxygen)#create_list 6) @ ((new Atom.carbon)#create_list 7))
end

class water =
object
	inherit molecule "Water" (((new Atom.hydrogen)#create_list 2) @ ((new Atom.oxygen)#create_list 1))
end

class carbon_dioxyde =
object
	inherit molecule "Carbon dioxyde" (((new Atom.carbon)#create_list 1) @ ((new Atom.oxygen)#create_list 2))
end

class trichloroisocyanuric_acid =
object
	inherit molecule "Trichloroisocyanuric acid" (((new Atom.carbon)#create_list 3) @ ((new Atom.chlorine)#create_list 3) @ ((new Atom.nitrogen)#create_list 3) @ ((new Atom.oxygen)#create_list 3))
end

class cycloserine =
object
	inherit molecule "Cycloserine" (((new Atom.carbon)#create_list 3) @ ((new Atom.hydrogen)#create_list 6) @ ((new Atom.nitrogen)#create_list 2) @ ((new Atom.oxygen)#create_list 2))
end

class glutamine_tritbdms =
object
	inherit molecule "Glutamine tritbdms" (((new Atom.carbon)#create_list 23) @ ((new Atom.hydrogen)#create_list 52) @ ((new Atom.nitrogen)#create_list 2) @ ((new Atom.oxygen)#create_list 3) @ ((new Atom.silicon)#create_list 3))
end
