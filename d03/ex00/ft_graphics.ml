type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
	if size < 1 then
		()
	else
	begin
		Graphics.moveto (x - (size / 2)) (y - (size / 2)) ;
		Graphics.lineto (x + (size / 2)) (y - (size / 2)) ;
		Graphics.lineto (x + (size / 2)) (y + (size / 2)) ;
		Graphics.lineto (x - (size / 2)) (y + (size / 2)) ;
		Graphics.lineto (x - (size / 2)) (y - (size / 2))
	end

let draw_square_and_string x y size to_print =
	Graphics.moveto x y ;
	Graphics.draw_string to_print ;
	draw_square x y size


let draw_tree_node tree =
	Graphics.moveto 600 300 ;
	match tree with
	| Nil -> ()
	| Node (content, left, right)
	->	Graphics.draw_string content ;
		draw_square 610 300 80 ;
		Graphics.moveto 650 300 ;
		Graphics.lineto 800 400 ;
		Graphics.moveto 650 300 ;
		Graphics.lineto 800 200 ;
		draw_square_and_string 840 400 80 "Nil" ;
		draw_square_and_string 840 200 80 "Nil"

let main () =
	Graphics.open_graph " 1200x700" ;
	draw_square 200 400 200 ;
	draw_tree_node (Node ("Pouet", Nil, Nil)) ;
	ignore ( Graphics.read_key () ) ;
	Graphics.close_graph ()

let _ = main ()
