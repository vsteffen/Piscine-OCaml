type t = Spade | Heart | Diamond | Club

let all = [ Spade ; Heart ; Diamond ; Club ]

let toString card =
	if card = Spade then
		"S"
	else if card = Heart then
		"H"
	else if card = Diamond then
		"D"
	else
		"C"

let toStringVerbose card =
	if card = Spade then
		"Spade"
	else if card = Heart then
		"Heart"
	else if card = Diamond then
		"Diamond"
	else
		"Club"
