(* let combine_left b row s1 s2 =
	let left = b.(row).(s1) + b.(row).(s2) in
	b.(row).(s1) <- Some left;
	b.(row).(s2) <- None

let combine_tiles b line s1 s2 m =
	match m with
	| Left -> combine_left b line s1 s2 *)