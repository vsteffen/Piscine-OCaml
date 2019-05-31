class virtual reaction molecule_in molecule_out =
object

	method virtual get_start : (Molecules.molecule * int) list
	method virtual get_result : (Molecules.molecule * int) list
	method virtual balance : reaction
	method virtual is_balanced : bool

end