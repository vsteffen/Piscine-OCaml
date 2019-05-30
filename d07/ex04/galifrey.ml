class galifrey (dalek : Dalek.dalek list) (doctor : Doctor.doctor list) (people : People.people list) =
	object
		val _dalek = dalek
		val _doctor = doctor
		val _people = people

	initializer Random.self_init () ;
	method do_time_war =
		let rec loop daleks doctors peoples =
			let next = function
				| [] -> []
				| head::tail -> tail
			in
			if List.length daleks = 0 then
				print_endline "Daleks army was defeated, doctors and people armies won the war!"
			else if List.length doctors = 0 && List.length peoples = 0 then
				print_endline "Doctors and people armies were defeated, Daleks army won the war!"
			else
			begin
				let rand = Random.int 2 in
				match rand with
				| 0 -> 
				begin
					let rand2 = Random.int 2 in
					match rand2 with
					| 0 -> print_endline ("Doctors and people killed " ^ ((List.hd daleks)#to_string)) ; loop (next daleks) doctors peoples
					| _ -> print_endline "Doctors and people failed to attack" ; loop daleks doctors peoples
				end
				| _ ->
				begin
					let rand2 = Random.int 3 in
					match rand2 with
					| 0 when List.length peoples > 0 -> print_endline ("Daleks killed " ^ ((List.hd peoples)#to_string)) ; loop daleks doctors (next peoples)
					| 1 when List.length doctors > 0 -> print_endline ("Daleks killed " ^ ((List.hd doctors)#to_string)) ; loop daleks (next doctors) peoples
					| _ -> print_endline "Daleks failed to attack" ; loop daleks doctors peoples
				end
			end
		in
		loop _dalek _doctor _people	
	end