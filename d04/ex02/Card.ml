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
	(** Interger representation of a card value, from 1 for T2 to 13 for As *)

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
	(** returns "2", ..., "10", "J", "Q", "K" or "A" **)

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
	(** returns "2", ..., "10", "Jack", "Queen", "King" or "As" **)

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



type t = ( Value.t * Color.t )

let newCard value color = ( value * color )

let allSpades =
	let createSpades value = ( Color.Spade , value ) in
	List.map createSpades Value.all

let allHearts =
	let createHearts value = ( Color.Heart , value ) in
	List.map createHearts Value.all

let allDiamonds =
	let createDiamonds value = ( Color.Diamond , value ) in
	List.map createDiamonds Value.all

let allClubs =
	let createClubs value = ( Color.Club , value ) in
	List.map createClubs Value.all

let getValue card = Value.t
let getColor card = Color.t

let toString card =
	let getValue ( value , _ ) = value in
	let getColor ( _ , value ) = color in
	Printf.sprintf ( "%s%s" , (getValue card).toString, (getColor card).toString )

let toStringVerbose card =
	let getValue ( value , _ ) = value in
	let getColor ( _ , value ) = color in
	Printf.sprintf ( "%s%s" , (getValue card).toStringVerbose, (getColor card).toStringVerbose )


let toCompare card1 card2 =
	let getValue ( value , _ ) = value in
	(getValue card1).toInt - (getValue card2).toInt

let max card1 card2 =
	let getValue ( value , _ ) = value in
	if (getValue card1).toInt < (getValue card2).toInt then
		card2
	else
		card1

let min card1 card2 =
	if (getValue card1).toInt > (getValue card2).toInt then
		card2
	else
		card1

let best cards =
	if cards = [] then
		invalid_arg ( "Invalid arg: there's no card !" )
	else
		let getValue ( value , _ ) = value in
		let findBest card best =
			if best < (getValue card).toInt then
				(getValue card).toInt
			else
				best
		in
		List.fold_left findBest cards 0

let isSpade card color =
	let getColor ( _ , color ) = color in
	( (getColor card) = color )

let isSpade card =
	let getColor ( _ , color ) = color in
	(color = Spade)

let isHeart card =
	let getColor ( _ , color ) = color in
	(color = Heart)

let isDiamond card =
	let getColor ( _ , color ) = color in
	(color = Diamond)

let isClub card =
	let getColor ( _ , color ) = color in
	(color = Club)
