let square_value v =
	match v with
	| None -> 0
	| Some i -> i

let combine_left b row s1 s2 =
	let left = (square_value (b.(row).(s1))) + (square_value (b.(row).(s2))) in
	b.(row).(s1) <- Some left;
	b.(row).(s2) <- None

let combine_tiles b line s1 s2 =
	combine_left b line s1 s2