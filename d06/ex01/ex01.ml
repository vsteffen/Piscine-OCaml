(* module StringHashtbl : STRINGHASHTBL
sig

end *)

module OwnHash =
struct
	type t = string
	let equal i j = i = j
	let hash i =
		let rec getFirstPart index acc =
			if index < 0 then
				acc
			else
			begin
				acc + (int_of_char (String.get i index))
			end
		in
		let rec getSecondPart index acc =
			if index < 0 then
				acc
			else
			begin
				acc + ((int_of_char (String.get i index)) * index)
			end
		in
		((getFirstPart ((String.length i) - 1) 0 ) mod 255) * ((getSecondPart ((String.length i) - 1) 0) mod 255)
		(* https://fr.wikipedia.org/wiki/Somme_de_contr%C3%B4le *)
end

module StringHashtbl = Hashtbl.Make (OwnHash)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
