module Color =
struct
	type t = Spade | Heart | Diamond | Club

	let all = [ Spade ; Heart ; Diamond ; Club ]

	let toString (card:t) =
		if card = Spade then
			"S"
		else if card = Heart then
			"H"
		else if card = Diamond then
			"D"
		else
			"C"

	let toStringVerbose (card:t) =
		if card = Spade then
			"Spade"
		else if card = Heart then
			"Heart"
		else if card = Diamond then
			"Diamond"
		else
			"Club"

end

module Value =
struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [ T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As ]

	let toInt card =
		match card with
		| T2 -> 1
		| T3 -> 2
		| T4 -> 3
		| T5 -> 4
		| T6 -> 5
		| T7 -> 6
		| T8 -> 7
		| T9 -> 8
		| T10 -> 9
		| Jack -> 10
		| Queen -> 11
		| King -> 12
		| As -> 13

	let toString card =
		match card with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "J"
		| Queen -> "Q"
		| King -> "K"
		| As -> "A"

	let toStringVerbose card =
		match card with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "Jack"
		| Queen -> "Queen"
		| King -> "King"
		| As -> "As"

	let next card =
	 	match card with
		| T2 -> T3
		| T3 -> T4
		| T4 -> T5
		| T5 -> T6
		| T6 -> T7
		| T7 -> T8
		| T8 -> T9
		| T9 -> T10
		| T10 -> Jack
		| Jack -> Queen
		| Queen -> As
		| _ -> invalid_arg "Invalid arg: No card after As"

	let previous card =
	 	match card with
		| As -> Queen
		| Queen -> Jack
		| Jack -> T10
		| T10 -> T9
		| T9 -> T8
		| T8 -> T7
		| T7 -> T6
		| T6 -> T5
		| T5 -> T4
		| T4 -> T3
		| T3 -> T2
		| _ -> invalid_arg "Invalid arg: No card before T2"

end



type t = Value.t * Color.t

let newCard value color = ( value , color )

let allSpades =
	let createSpades value = ( value , Color.Spade) in
	List.map createSpades Value.all

let allHearts =
	let createHearts value = ( value , Color.Heart) in
	List.map createHearts Value.all

let allDiamonds =
	let createDiamonds value = ( value , Color.Diamond) in
	List.map createDiamonds Value.all

let allClubs =
	let createClubs value = ( value , Color.Club) in
	List.map createClubs Value.all

let all =
	allClubs @ allDiamonds @ allHearts @ allSpades

let getValue = fun ( value , _ ) -> value
let getColor = fun ( _ , color ) -> color

let toString card =
	Printf.sprintf "%s%s" (Value.toString (getValue card)) (Color.toString (getColor card))

let toStringVerbose card =
	Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose (getValue card))  (Color.toStringVerbose (getColor card))


let compare card1 card2 =
	(Value.toInt (getValue card1)) - (Value.toInt (getValue card2))

let max card1 card2 =
	if Value.toInt (getValue card1) >= Value.toInt (getValue card2) then
		card1
	else
		card2

let min card1 card2 =
	if Value.toInt (getValue card1) <= Value.toInt (getValue card2) then
		card1
	else
		card2

let best cards =
	if cards = [] then
		invalid_arg ( "Invalid arg: there's no card !" )
	else
		let findBest card best =
			if Value.toInt (getValue card) < Value.toInt (getValue best) then
				card
			else
				best
		in
		List.fold_left findBest (List.hd cards) cards


let isOf card color =
	getColor card = color

let isSpade card =
	getColor card = Color.Spade

let isHeart card =
	getColor card = Color.Heart

let isDiamond card =
	getColor card = Color.Diamond

let isClub card =
	getColor card = Color.Club
