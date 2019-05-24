type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_not_empty = function
	| _ -> false

let is_bst tree =
	let rec iter_tree tree min max =
		match tree with
		| Nil -> true
		| Node (n as ntm, left, right) when
			((
				match max with
				| (is_init, _) when not is_init -> true
				| (_, max) when max > ntm -> true
				| _ -> false
			)
			&& 
			(
				match min with
				| (is_init, _) when not is_init -> true
				| (_, min) when min < n -> true
				| _ -> false
			))
			-> (left == Nil || (iter_tree left min (true, n)) && (right == Nil || iter_tree right (true, n) max))
		| _ -> false
	in
	iter_tree tree (false, 0) (false, 0)

let is_perfect tree =
	let rec find_deep tree deep =
		match tree with
		| Node (_, Nil, Nil) -> deep
		| Node (_, (Node (_, _, _) as left), _) -> find_deep left (deep + 1)
		| _ -> 0
	in
	let rec iter_tree tree curr deep =
		match tree with
		| Node (_, Nil, Nil) -> if (curr == deep) then true else false
		| Node (_, left, right) -> (left == Nil || (iter_tree left (curr + 1) deep)) && (right == Nil || (iter_tree right (curr + 1) deep))
		| _ -> false
	in
	iter_tree tree 0 (find_deep tree 0)


let is_balanced tree =
	let rec iter_tree tree =
		match tree with
		| Node (_, Nil, Node(_, Node(_, _, _), Nil)) -> false
		| Node (_, Nil, Node(_, Nil, Node(_, _, _))) -> false
		| Node (_, Nil, Node(_, Node(_, _, _), Node(_, _, _))) -> false
		| Node (_, Node(_, Node(_, _, _), Nil), Nil) -> false
		| Node (_, Node(_, Nil, Node(_, _, _)), Nil) -> false
		| Node (_, Node(_, Node(_, _, _), Node(_, _, _)), Nil) -> false
		| Node (_, left, right) -> ((left == Nil || (iter_tree left)) && (right == Nil || (iter_tree right)))
		| _ -> true
	in
	iter_tree tree

let main () =
	let tree1 =
		Node (
			42,
			Node (
				12,
				Node (9, Nil, Nil ),
				Node (16, Nil, Nil )
			),
			Node (
				54,
				Node (
					46,
					Node (44, Nil, Nil ),
					Node (48, Nil, Nil )
				),
				Node (
					70,
					Node (
						66,
						Node (60, Nil, Nil ),
						Node (68, Node (67, Nil, Nil ), Node (69, Nil, Nil ) )
					),
					Node (78, Nil, Nil)
				)
			)
		)
	in
	let tree2 =
		Node (
			42,
			Node (
				12,
				Node (9, Nil, Nil ),
				Node (43, Nil, Nil )
			),
			Node (
				55,
				Node (55, Nil, Nil ),
				Node (65, Nil, Nil )
			)
		)
	in
	let tree3 =
		Node (
			8,
			Node (
				3,
				Node (1, Nil, Nil ),
				Node (
					6,
					Node (4, Nil, Nil ),
					Node (7, Nil, Nil ))
			),
			Node (
				10,
				Nil,
				Node (14, Node (13, Nil, Nil ), Nil )
			)
		)
	in
	let tree4 =
		Node (
			8,
			Node (
				3,
				Node (1, Nil, Nil ),
				Node (
					6,
					Node (4, Nil, Nil ),
					Node (7, Nil, Nil ))
			),
			Node (
				13,
				Node (10, Nil, Nil ),
				Node (14, Nil, Nil )
			)
		)
	in
	print_endline (string_of_bool (is_bst tree1)) ;
	print_endline (string_of_bool (is_perfect tree1)) ;
	print_endline (string_of_bool (is_perfect tree2)) ;
	print_endline (string_of_bool (is_balanced tree3)) ;
	print_endline (string_of_bool (is_balanced tree4))

let () = main ()
