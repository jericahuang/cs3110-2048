let square_value v =
	match v with
	| None -> 0
	| Some i -> i

let combine_left b s row s1 s2 =
	let left = (square_value (b.(row).(s1))) + (square_value (b.(row).(s2))) in
	s := !s + left;
	b.(row).(s1) <- Some left;
	b.(row).(s2) <- None

let combine_tiles b s line s1 s2 =
	combine_left b s line s1 s2

let replace_tile b row col sq = 
	b.(row).(col) <- sq