open Types

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

(* shifts two tiles [s1] and [s2] to the left and accounts for new open space 
   generated by the shift *)
let rec shift_left b row s1 s2 =
  b.(row).(s1) <- b.(row).(s2);
  b.(row).(s2) <- None

(* when moving the board, fix_row will remove all of the blanks of each row to
   make merging easier *)
let rec fix_row b row size =
  if size = 0 then () else
  (if b.(row).(size-1) = None && b.(row).(size) <> None
  then shift_left b row (size-1) size else ();
  if size <> 3 && b.(row).(size) = None && b.(row).(size+1) <> None
  then fix_row b row (size+1) else ();
  fix_row b row (size-1))

(* Returns whether or not row [r] is empty in board [b] of 
 * length/width [size] *)
let rec is_empty_row (b : board) (row : int) (size : int) =
  if size = 0 then true else
  if b.(row).(size-1) = None then 
    is_empty_row b row (size-1) else false

let is_valid_merge_horizontal b row s1 s2 =
  if b.(row).(s1) <> None then 
    b.(row).(s1) = b.(row).(s2) else false

  (* process_row will look through a row and look for a valid merge and if there 
   is one it will combine the tiles together *)
let rec process_row b s row col size =
  fix_row b row (size-1);
  if col > 3 then () else
  (if col <> 3 && is_valid_merge_horizontal b row col (col+1)
  then combine_tiles b s row col (col+1) else ();
  process_row b s row (col+1) size)

(* works to complete all actions of a move (shifting and merging tiles and 
   incrmenting the score accordingly) in the leftward direction as long as 
   the row is not empty *)
let rec move_left b s row col size =
  if row = 0 then () else
  (if is_empty_row b (row-1) size then () else
  process_row b s (row-1) 0 size;
  move_left b s (row-1) col size)
