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
