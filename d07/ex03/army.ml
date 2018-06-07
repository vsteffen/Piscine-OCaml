class ['a] army (army:'a list) =
	object
		val _army : 'a list = army
		method get_army = _army
		method add (elem : 'a) = {< _army = elem :: _army >}
		method delete =
			match _army with
			| [] -> {< _army = [] >}
			| (head:'a)::(tail:'a list) -> {< _army = tail >}
	end
