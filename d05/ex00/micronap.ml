let my_sleep () = Unix.sleep 1

(* ocamlopt -o micronap unix.cmxa micronap.ml  *)

let () =
    if Array.length Sys.argv = 2 then
	begin
        let get_time_sleep =
			try int_of_string (Array.get Sys.argv 1) with Failure (_) -> 0
		in
        for i = 1 to get_time_sleep do
			my_sleep ()
		done
	end
	else
		print_endline "Usage: micronap [TIME TO SLEEP]"
