open Types


(* [square_value t] is [t]'s value (if any). *)
let square_value v =
	match v with
	| None -> 0
	| Some i -> i


(* [combine_tiles c r f] replaces the [r][c] element with
 * what is returned from applying f to the [r][c] element. Note [r] is a
 * list while [c] is an int that represent the index within the list *)
let combine_tiles b s row s1 s2 =
	let left = (square_value (b.(row).(s1))) + (square_value (b.(row).(s2))) in
	s := !s + left;
	b.(row).(s1) <- Some left;
	b.(row).(s2) <- None


(* [shift_left b row s1 s2] shifts two tiles [s1] and [s2] to the left and 
 * accounts for new open space generated by the shift *)
let rec shift_left b row s1 s2 =
  b.(row).(s1) <- b.(row).(s2);
  b.(row).(s2) <- None


(* [fix_row b row size] will remove all of the blanks of a [row] to make merging 
 * easier when a move is made on the board *)
let rec fix_row b row size =
  if size = 0 then () else
  (if b.(row).(size-1) = None && b.(row).(size) <> None
  then shift_left b row (size-1) size else ();
  if size <> 3 && b.(row).(size) = None && b.(row).(size+1) <> None
  then fix_row b row (size+1) else ();
  fix_row b row (size-1))


(* [is_empty_row b row size] returns whether or not row [r] is empty in board [b] of 
 * length/width [size] *)
let rec is_empty_row b row size =
  if size = 0 then true else
  if b.(row).(size-1) = None then 
    is_empty_row b row (size-1) else false


(* [is_valid_merge_vertical b col s1 s2] returns a bool that 
 * indicates whether or not a valid up or down
 * move is valid given two squares [s1] and [s2] and a [col] *)
let is_valid_merge_horizontal b row s1 s2 =
  if b.(row).(s1) <> None then 
    b.(row).(s1) = b.(row).(s2) else false


(* [is_valid_merge_horizontal b row s1 s2] returns a bool that 
 * indicates whether or not a valid left or right
 * move is valid given two squares [s1] and [s2] and a [row] *)
let is_valid_merge_vertical b col r1 r2 =
  if b.(r1).(col) <> None then
    b.(r1).(col) = b.(r2).(col) else false


(* [process_row b s row col size] will look through a [row] and look for a valid merge 
 * and if there is one it will combine the tiles together *)
let rec process_row b s row col size =
  fix_row b row (size-1);
  if col > 3 then () else
  (if col <> 3 && is_valid_merge_horizontal b row col (col+1)
  then combine_tiles b s row col (col+1) else ();
  process_row b s row (col+1) size)


(* [move_left b s row col size] works to complete all actions of a move (shifting 
 * and merging tiles and incrementing the score accordingly) in the leftward direction
 * as long as the row is not empty *)
let rec move_left b s row col size =
  if row = 0 then () else
  (if is_empty_row b (row-1) size then () else
  process_row b s (row-1) 0 size;
  move_left b s (row-1) col size)


(* [to_lst b size] converts the board [b] to a matrix list *)
let rec to_lst b size =
  if size = 4 then [] else
  Array.to_list b.(size) :: to_lst b (size+1)

(* [to_lst_rev b size] converts the board to a matrix list 
 * but each row is in reverse order *)
let rec to_lst_rev b size =
  if size = 4 then [] else
  (List.rev (Array.to_list b.(size))) :: to_lst_rev b (size+1)

(* [to_arr] converts the matrix list [lst] back into a valid board array *)
let rec to_arr lst arr size =
  match lst with
  | [] -> ()
  | h::t -> arr.(size) <- (Array.of_list h); to_arr t arr (size+1)

(* [get_head] gets the first value of a row *)
let rec get_head lst =
  match lst with
  | [] -> []
  | []::[] -> []
  | []::(h::t) -> h
  | (h::t) :: t' -> h :: get_head t'

(* [get_tail] gets the rest of the row excluding the first value *)
let rec get_tail lst =
  match lst with
  | [] -> []
  | []::[] -> []
  | []::(h::t) -> t
  | (h::t) :: t' -> t :: get_tail t'

(* [rotate lst] rotates the board so that move_left can be applied to 
 * all moves using the same function *)
let rec rotate lst =
  match lst with
  | [] -> []
  | []::_ -> []
  | (h::t) :: t' -> (h :: get_head t') :: rotate (t :: get_tail t')

(* [rotate_up b] rotates the board 90 degrees counter-clockwise *)
let rotate_up b =
  to_arr (rotate (to_lst b 0)) b 0

(* [rotate_right b] rotates the board to be in reverse order *)
let rotate_right b =
  to_arr (to_lst_rev b 0) b 0

(* [move m b s] moves the board [b] in direction [m] *)
let move m b s =
  match m with
  | Left -> move_left b s
            (Array.length b.(0)) (Array.length b) (Array.length b)
  | Right -> rotate_right b;
             move_left b s
            (Array.length b) (Array.length b) (Array.length b);
             rotate_right b
  | Up -> rotate_up b;
          move_left b s
          (Array.length b) (Array.length b) (Array.length b);
          rotate_up b
  | Down -> rotate_up b; rotate_right b;
            move_left b s
            (Array.length b) (Array.length b) (Array.length b);
            rotate_right b; rotate_up b
  | (Regular|Evil|Null) -> ()
