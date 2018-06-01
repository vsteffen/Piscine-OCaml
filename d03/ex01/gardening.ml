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
	Graphics.moveto (x - 10 ) y ;
	Graphics.draw_string to_print ;
	draw_square x y size

let draw_join_square_up x y size width_stair height_stair =
	Graphics.moveto (x + size / 2) y ;
	Graphics.lineto ((x - size / 2) + width_stair) (y + height_stair)

let draw_join_square_down x y size width_stair height_stair =
	Graphics.moveto (x + size / 2) y ;
	Graphics.lineto ((x - size / 2) + width_stair) (y - height_stair)


let node_exist node =
	match node with
	| Nil -> false
	| Node (content, left, right) -> true

let draw_tree tree =
	let rec explore_tree tree x y sep_height =
		match tree with
		| Nil -> ()
		| Node (content, left, right)
		->	draw_square_and_string x y 50 content ;
			if node_exist left then
				draw_join_square_up x y 50 160 sep_height ;
			if node_exist right then
				draw_join_square_down x y 50 160 sep_height ;
			explore_tree left (x + 160) (y + (sep_height)) (sep_height / 2) ;
			explore_tree right (x + 160) (y - (sep_height)) (sep_height / 2)
	in
	explore_tree tree 250 500 (500 / 2)


let rec size tree =
	match tree with
	| Nil -> 0
	| Node (content, left, right) -> (size left) + 1 + (size right)

let rec height tree =
	match tree with
	| Nil -> 0
	| Node (content, left, right)
	->	if (height left) > (height right) then
			((height left) + 1)
		else
			((height right) + 1)


let main () =
	let tree1 =
		Node (
			"42",
			Node (
				"1",
				Node ("10", Nil, Nil ),
				Node ("11", Nil, Nil )
			),
			Node (
				"0",
				Node (
					"01",
					Node ("011", Nil, Nil ),
					Node ("010", Nil, Nil )
				),
				Node (
					"00",
					Node (
						"001",
						Node ("0011", Nil, Nil ),
						Node ("0010", Nil, Nil )
					),
					Node ("000", Nil, Nil)
				)
			)
		)
	in
	let tree2 = Node ("Pouet", Nil, Nil) in
	let tree3 = Node ("LOL", Node ("MDR", Nil, Nil), Nil) in

	print_endline ( "Size of trees (tree1, tree2, tree3):" );
	print_int ( size tree1 ); print_char '\n' ;
	print_int ( size tree2 ); print_char '\n' ;
	print_int ( size tree3 ); print_char '\n' ;
	print_char '\n' ;
	print_endline ( "Height of trees (tree1, tree2, tree3):" );
	print_int ( height tree1 ); print_char '\n' ;
	print_int ( height tree2 ); print_char '\n' ;
	print_int ( height tree3 ); print_char '\n' ;

	Graphics.open_graph " 1200x1000" ;
	draw_tree tree1 ;
	ignore ( Graphics.read_key () );
	Graphics.close_graph () ;

	Graphics.open_graph " 1200x1000" ;
	draw_tree tree2 ;
	ignore ( Graphics.read_key () );
	Graphics.close_graph () ;

	Graphics.open_graph " 1200x1000" ;
	draw_tree tree3 ;
	ignore ( Graphics.read_key () );
	Graphics.close_graph ()

let _ = main ()
