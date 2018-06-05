let main () =
	let basicCard1 = Card.newCard Card.Value.T3 Card.Color.Club in
	let basicCard2 = Card.newCard Card.Value.Jack Card.Color.Spade in
	let basicCard3 = Card.newCard Card.Value.As Card.Color.Heart in
	let basicCard4 = Card.newCard Card.Value.T9 Card.Color.Diamond in

	print_endline ( "Card -> toString / toStringVerbose:" ) ;
	print_endline ( Card.toString (basicCard1) ) ;
	print_endline ( Card.toStringVerbose (basicCard1) ) ;

	print_endline ( "\nCard -> allClubs / allSpades / allHearts / allHearts / all:" ) ;
	List.iter (function card -> print_endline (Card.toStringVerbose card) ;) Card.all ;
	(* List.iter (function card -> print_endline (Card.toStringVerbose card) ;) Card.allSpades ;
	List.iter (function card -> print_endline (Card.toStringVerbose card) ;) Card.allHearts ;
	List.iter (function card -> print_endline (Card.toStringVerbose card) ;) Card.allDiamonds ;
	List.iter (function card -> print_endline (Card.toStringVerbose card) ;) Card.allClubs ; *)

	print_endline ( "\nCard -> getValue / getColor:" ) ;
	print_endline ( Card.Value.toStringVerbose (Card.getValue basicCard1 )) ;
	print_endline ( Card.Color.toStringVerbose (Card.getColor basicCard1 )) ;

	print_endline ( "\nCard -> compare / max / min / best:" ) ;
	print_int (Card.compare basicCard2 basicCard3 ) ; print_char '\n' ;
	print_int (Card.compare basicCard3 basicCard2 ) ; print_char '\n' ;
	print_int (Card.compare basicCard2 basicCard2 ) ; print_char '\n' ;
	print_endline ( Card.toStringVerbose (Card.max basicCard2 basicCard3) ) ;
	print_endline ( Card.toStringVerbose (Card.min basicCard2 basicCard3) ) ;
	print_endline ( Card.toStringVerbose (Card.best [basicCard2 ; basicCard3]) ) ;
	(* print_endline ( Card.toStringVerbose (Card.best []) ) ; *)

	print_endline ( "\nCard -> isOf / isSpade / isHeart / isDiamond / isClub:" ) ;
	print_endline ( string_of_bool (Card.isOf basicCard4 Card.Color.Diamond) ) ;
	print_endline ( string_of_bool (Card.isSpade basicCard4) ) ;
	print_endline ( string_of_bool (Card.isSpade basicCard2) )


let () = main ()
