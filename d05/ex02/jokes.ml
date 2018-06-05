let () =
	Random.self_init ();
	let arrayOfJokes =
	[|
		"Qui est-ce qui court et qui se jette ?\nUne courgette." ;
		"C'est l'histoire d'un schtroumpf qui court, qui tombe\n... et qui se fait un bleu.";
		"Méfiez vous de l’oiseau sur le lac :\nC’est peut-être un mauvais cygne !";
		"Une carotte veut se suicider. Hélas, elle échoue et puis se dit :\n- Zut, c'est râpé !";
		"Deux bonbons qui marchent sur la route. Un flic les arrête et dit : \n- Vos papiers s'il vous plaît !!"
	|] in
	let random_int = Random.int 5 in
	print_endline (Array.get (arrayOfJokes) (random_int))
