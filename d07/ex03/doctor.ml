class doctor name age (sidekick : People.people) =
	object (self)
		val _name = name
		val _hp = 100
		val _age = age
		val _sidekick = sidekick
		initializer print_endline "The way I see it, every life is a pile of good things and bad things. The good things don’t always soften the bad things, but vice versa, the bad things don’t always spoil the good things and make them unimportant."
		method to_string = "Doctor -> _name = " ^ _name ^ " | _hp = " ^ string_of_int (_hp) ^ " | _age = " ^ string_of_int (_age) ^ " | _sidekick = " ^ _sidekick#to_string
		method talk = "Hi! I’m the Doctor!"
		method travel_in_time start arrival =
			if _age - (start - arrival) < 0 then
			begin
				print_endline "I can't use the Tardis or i will break space-time!" ;
				{< _name = _name ; _hp = _hp ; _age = _age ; _sidekick = _sidekick >}
			end
			else
			begin
				print_endline "
            ___
            | |
            | |
    -------------------
    -------------------
     |  ___  |  ___  |
     | | | | | | | | |
     | |-+-| | |-+-| |
     | |_|_| | |_|_| |
     |  ___  |  ___  |
     | |   | | |   | |
     | |   | | |   | |
     | |___| | |___| |
     |  ___  |  ___  |
     | |   | | |   | |
     | |   | | |   | |
     | |___| | |___| |
     |       |       |
    ===================
			" ;
			{< _name = _name ; _hp = _hp ; _age = _age - (start - arrival) ; _sidekick = _sidekick >}
			end

		method use_sonic_screwdriver = "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
		method private regenerate = {< _name = _name ; _hp = 100 ; _age = _age ; _sidekick = _sidekick >}
		method weird_subject_take_dmg dmg =
			if _hp - dmg < 0 then
				self#regenerate
			else
				{< _name = _name ; _hp = (_hp - dmg) ; _age = _age ; _sidekick = _sidekick >}
	end
