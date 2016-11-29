
exception End_game

type move =
  | Left
  | Right
  | Up
  | Down
(* type square = int option *)
(* type row = square array
type board = row array *)

(* We may be able to add more to this *)
type square = {
  mutable value: int option;
}

type board = square array array

(* let empty = None *)
(* let t2 = Some 2
let t4 = Some 4
let t8 = Some 8
let t16 = Some 16
let t32 = Some 32
let t64 = Some 64
let t128 = Some 128
let t256 = Some 256
let t512 = Some 512
let t1028 = Some 1028
let t2048 = Some 2048 *)


let square_value v =
	match v with
	| None -> 0
	| Some i -> i

(* [is_empty_square s] checks if [s] is an empty square. *)
let is_empty_square (s : square) =
  square_value s.value = 0

(* [init_board size] initializes the board with [size].
 * Starts with square of 2 in bottom left corner.
 * requires: size >= 1 *)
 (* right now deafult to 4. Change to size and size - 1 *)
let init_board size =
  if size < 1 then failwith "Invalid matrix size"
  else
    let b = Array.make_matrix 4 4 {value = None} in
    b.(3).(3) <- {value = (Some 2)}; b

(* [check_2048_sqaure s] returns if 2048 square has been formed. *)
let check_2048_square (s : square) =
  square_value s.value = 2048

let rec is_empty_row b row size = 
  if size = 0 then true else
  if b.(row).(size-1) = None then is_empty_row b row (size-1) else false

let rec is_empty_col b col size = 
  if size = 0 then true else
  if b.(size-1).(col) = None then is_empty_col b col (size-1) else false

let is_valid_merge_horizontal b row s1 s2 = 
  if b.(row).(s1) <> None then b.(row).(s1) = b.(row).(s2) else false

let is_valid_merge_vertical b col r1 r2 = 
  if b.(r1).(col) <> None then b.(r1).(col) = b.(r2).(col) else false 

let rec is_valid_move_left b row col =
  if row = 0 then false else
  if is_empty_row b (row-1) col then is_valid_move_left b (row-1) col else
  if b.(row-1).(0) = None then true else
  if b.(row-1).(0) <> None && b.(row-1).(1) = None && b.(row-1).(2) <> None then true else
  if b.(row-1).(0) <> None && b.(row-1).(1) <> None && b.(row-1).(2) = None && b.(row-1).(3) <> None then true else
  if is_valid_merge_horizontal b (row-1) 0 1 || is_valid_merge_horizontal b (row-1) 1 2 || is_valid_merge_horizontal b (row-1) 2 3 
  then true else is_valid_move_left b (row-1) col

let rec is_valid_move_right b row col =
  if row = 0 then false else
  if is_empty_row b (row-1) col then is_valid_move_right b (row-1) col else
  if b.(row-1).(3) = None then true else
  if b.(row-1).(3) <> None && b.(row-1).(2) = None && b.(row-1).(1) <> None then true else
  if b.(row-1).(3) <> None && b.(row-1).(2) <> None && b.(row-1).(1) = None && b.(row-1).(0) <> None then true else
  if is_valid_merge_horizontal b (row-1) 0 1 || is_valid_merge_horizontal b (row-1) 1 2 || is_valid_merge_horizontal b (row-1) 2 3 
  then true else is_valid_move_right b (row-1) col

let rec is_valid_move_up b row col = 
  if col = 0 then false else
  if is_empty_col b (col-1) row then is_valid_move_up b row (col-1) else
  if b.(0).(col-1) = None then true else 
  if b.(0).(col-1) <> None && b.(1).(col-1) = None && b.(2).(col-1) <> None then true else
  if b.(0).(col-1) <> None && b.(1).(col-1) <> None && b.(2).(col-1) = None && b.(3).(col-1) <> None then true else
  if is_valid_merge_vertical b (col-1) 0 1 || is_valid_merge_vertical b (col-1) 1 2 || is_valid_merge_vertical b (col-1) 2 3
  then true else is_valid_move_up b row (col-1)

let rec is_valid_move_down b row col = 
  if col = 0 then false else
  if is_empty_col b (col-1) row then is_valid_move_down b row (col-1) else
  if b.(3).(col-1) = None then true else 
  if b.(3).(col-1) <> None && b.(2).(col-1) = None && b.(1).(col-1) <> None then true else
  if b.(3).(col-1) <> None && b.(2).(col-1) <> None && b.(1).(col-1) = None && b.(0).(col-1) <> None then true else
  if is_valid_merge_vertical b (col-1) 0 1 || is_valid_merge_vertical b (col-1) 1 2 || is_valid_merge_vertical b (col-1) 2 3
  then true else is_valid_move_down b row (col-1)


let is_valid_move m b = 
  match m with
  | Left -> is_valid_move_left b (Array.length b) (Array.length b)
  | Right -> is_valid_move_right b (Array.length b) (Array.length b)
  | Up -> is_valid_move_up b (Array.length b) (Array.length b)
  | Down -> is_valid_move_down b (Array.length b) (Array.length b)

let combine_left b row s1 s2 =
  let left = (square_value (b.(row).(s1))) + (square_value (b.(row).(s2))) in
  b.(row).(s1) <- Some left;
  b.(row).(s2) <- None

let combine_tiles b line s1 s2 =
  combine_left b line s1 s2

let rec shift_left b row s1 s2 = 
  b.(row).(s1) <- b.(row).(s2);
  b.(row).(s2) <- None

let rec fix_row b row size = 
  if size = 0 then () else
  (if b.(row).(size-1) = None && b.(row).(size) <> None 
  then shift_left b row (size-1) size else ();
  if size <> 3 && b.(row).(size) = None && b.(row).(size+1) <> None 
  then fix_row b row (size+1) else (); 
  fix_row b row (size-1))

let rec process_row b row col size = 
  fix_row b row (size-1);
  if col > 3 then () else 
  (if col <> 3 && is_valid_merge_horizontal b row col (col+1)
  then combine_tiles b row col (col+1) else ();
  process_row b row (col+1) size)

let rec move_left b row col size = 
  if row = 0 then () else
  (if is_empty_row b (row-1) size then () else
  process_row b (row-1) 0 size;
  move_left b (row-1) col size) 

let rec to_lst b size = 
  if size = 4 then [] else
  Array.to_list b.(size) :: to_lst b (size+1)

let rec to_arr lst arr size = 
  match lst with
  | [] -> arr
  | h::t -> arr.(size) <- (Array.of_list h); to_arr t arr (size+1)

let rec get_head lst = 
  match lst with 
  | [] -> []
  | (h::t) :: t' -> h :: get_head t'

let rec get_tail lst = 
  match lst with
  | [] -> []
  | (h::t) :: t' -> t :: get_tail t'

let rec rotate lst = 
  match lst with
  | [] -> []
  | []::_ -> []
  | (h::t) :: t' -> (h :: get_head t') :: rotate (t :: get_tail t')

let rotate_up b =
  to_arr (rotate (to_lst b 0)) b 0

let rotate_right b = 
  to_arr (rotate (rotate (to_lst b 0))) b 0

let rotate_down b = 
  to_arr (rotate (rotate (rotate (to_lst b 0)))) b 0

let move m b = 
  match m with 
  | Left -> move_left b (Array.length b) (Array.length b) (Array.length b)
  | Right -> move_left b (Array.length b) (Array.length b) (Array.length b);
             rotate_right b; ()
  | Up -> move_left b (Array.length b) (Array.length b) (Array.length b);
          rotate_down b; ()
  | Down -> move_left b (Array.length b) (Array.length b) (Array.length b);
            rotate_up b; ()

let keyup m b = 
  if is_valid_move m b then 
  (match m with
    | Left -> move m b
    | Right -> move m (rotate_right b)
    | Up -> move m (rotate_up b)
    | Down -> move m (rotate_down b))
  else ()

(*let check_winning_board (b : board) =
  let win = ref false in
  for i = 0 to (Array.length b) - 1 do
    if Array.exists check_2048_square b.(i) then win := true
  done;
  !win
  

(* Random square insertion *)

(* Returns a random member of list [l] *)
let random_nth_list l  =
  let len = List.length l in
  List.nth l (Random.int len)

let (>>=) l f = List.concat (List.map f l)

let list_index = [0;1;2;3]

(* Returns a tuple (i,j) of a random open position in [b] in row i, column j*)
let random_avail b =
  let all_indicies =
    list_index >>= fun i ->
    list_index >>= fun j ->
    [(i, j)]
  in
  let avail = List.filter (fun (i, j) -> b.(i).(j) = None) all_indicies in
  if avail <> [] then
    random_nth_list avail 
  else
    raise (Failure "full board")

(* Inserts pre-determined square [sq] into board [b] *)
let insert_square (sq : square) (b : board) : board =
  let (i, j) = random_avail b in
  b.(i).(j) <- sq


(* ASSUMING FUNCTIONALITY *)
let check_end_game (b : board) =
  let lboard = move_left b in
  let rboard = move_right b in
  let uboard = move_up b in
  let dboard = move_down b in
  let same_b = (b = lboard && b = rboard && b = uboard && b = dboard) in
  if same_b then raise End_game
  else b*)
