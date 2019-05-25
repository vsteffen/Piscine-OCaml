let main () =
    Random.self_init () ;
    let rec draw_all_cards_in_deck deck =
        let (card, deck) = Deck.drawCard deck in
        print_endline ("Draw card: " ^ (Deck.Card.toString card));
        draw_all_cards_in_deck deck
    in
    let deck = Deck.newDeck () in
    print_endline "Deck.toStringList :" ;
    List.iter print_endline (Deck.toStringList deck);
    print_endline "\nDeck.toStringListVerbose:" ;
    List.iter print_endline (Deck.toStringListVerbose deck);
    print_char '\n';
    draw_all_cards_in_deck deck

let () = main ()
