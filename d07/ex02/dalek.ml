class dalek name =
	object (self)
		val _name =
			let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
			let getRandomCharToString rand = String.make 1 (String.get alphabet ((Random.int rand) mod 52)) in
			let rec loop i name =
				if i < 3 then
				begin
					Random.self_init () ;
					loop (i + 1) (name ^ (getRandomCharToString (52 + i)))
				end
				else
					name
			in
			loop 0 name
		val _hp = 100
		val mutable _shield = true
		method to_string = "dalek -> _name = " ^ _name ^ " | _hp = " ^ (string_of_int _hp) ^ " | _shield = " ^ (string_of_bool _shield)
		method talk =
			let quotesList = [
				"Explain! Explain!" ;
				"Exterminate! Exterminate!" ;
				"I obey!" ;
				"You are the Doctor! You are the enemy of the Daleks!"
			] in
			List.nth quotesList (Random.int 4)
		method exterminate (to_kill : People.people) = _shield <- false ; to_kill#die
		method die = "Emergency Temporal Shift!"
		method take_dmg dmg =
			if _hp - dmg < 0 then
				{< _name = _name ; _hp = 0 ; _shield = _shield >}
			else
				{< _name = _name ; _hp = (_hp - dmg) ; _shield = _shield >}

	end
