class people name =
	object
		val _name = name
		val _hp = 100
		initializer print_endline "Letting it get to you. You know what that's called? Being alive. Best thing there is. Being alive right now is all that counts."
		method to_string = "Name : " ^ _name ^ " | hp = " ^ string_of_int (_hp)
		method talk = "I’m " ^ _name ^ "! Do you know the Doctor?"
		method die = "Aaaarghh!"
	end
