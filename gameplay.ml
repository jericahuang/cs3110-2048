open Types
open Gamelogic

let () = Random.self_init ()

(* [is_empty_square s] checks if [s] is an empty square. *)
let is_empty_square (s : square) =
  square_value s = 0

(* [init_board size] initializes the board with [size].
 * Starts with square of 2 in bottom left corner.
 * requires: size >= 1 *)
 (* right now deafult to 4. Change to size and size - 1 *)
let init_board size =
  if size < 1 then failwith "Invalid matrix size"
  else
    let b = Array.make_matrix 4 4 None in
    let s = ref 0 in
    let e = ref false in
    b.(3).(3) <- Some 2;
    {
      evil = e;
      s = s;
      b = b;
    }

(* [check_2048_square s] returns if 2048 square has been formed. *)
let check_2048_square (s : square) =
  square_value s = 2048

(* Returns whether or not column [col] is empty in board [b] of
 * length/width [size] *)
let rec is_empty_col b col size =
  if size = 0 then true else
  if b.(size-1).(col) = None then
    is_empty_col b col (size-1) else false

let rec is_valid_move_left b row col =
  if row = 0 then false else
  if is_empty_row b (row-1) col then is_valid_move_left b (row-1) col else
  if b.(row-1).(0) = None then true else
  if b.(row-1).(0) <> None && b.(row-1).(1) = None
  && (b.(row-1).(2) <> None || b.(row-1).(3) <> None) then true
  else if b.(row-1).(0) <> None && b.(row-1).(1) <> None && b.(row-1).(2) = None
  && b.(row-1).(3) <> None then true
  else if is_valid_merge_horizontal b (row-1) 0 1
  || is_valid_merge_horizontal b (row-1) 1 2
  || is_valid_merge_horizontal b (row-1) 2 3
  then true else is_valid_move_left b (row-1) col

let rec is_valid_move_right b row col =
  if row = 0 then false else
  if is_empty_row b (row-1) col then is_valid_move_right b (row-1) col else
  if b.(row-1).(3) = None then true else
  if b.(row-1).(3) <> None && b.(row-1).(2) = None && (b.(row-1).(1) <> None
  || b.(row-1).(0) <> None) then true else
  if b.(row-1).(3) <> None && b.(row-1).(2) <> None && b.(row-1).(1) = None
  && b.(row-1).(0) <> None then true else
  if is_valid_merge_horizontal b (row-1) 0 1
  || is_valid_merge_horizontal b (row-1) 1 2
  || is_valid_merge_horizontal b (row-1) 2 3
  then true else is_valid_move_right b (row-1) col

let rec is_valid_move_up b row col =
  if col = 0 then false else
  if is_empty_col b (col-1) row then is_valid_move_up b row (col-1) else
  if b.(0).(col-1) = None then true else
  if b.(0).(col-1) <> None && b.(1).(col-1) = None && (b.(2).(col-1) <> None
  || b.(3).(col-1) <> None) then true
  else if b.(0).(col-1) <> None && b.(1).(col-1) <> None
  && b.(2).(col-1) = None && b.(3).(col-1) <> None then true
  else if is_valid_merge_vertical b (col-1) 0 1
  || is_valid_merge_vertical b (col-1) 1 2
  || is_valid_merge_vertical b (col-1) 2 3
  then true else is_valid_move_up b row (col-1)

let rec is_valid_move_down b row col =
  if col = 0 then false else
  if is_empty_col b (col-1) row then is_valid_move_down b row (col-1) else
  if b.(3).(col-1) = None then true else
  if b.(3).(col-1) <> None && b.(2).(col-1) = None && (b.(1).(col-1) <> None
  || b.(0).(col-1) <> None) then true
  else if b.(3).(col-1) <> None && b.(2).(col-1) <> None && b.(1).(col-1) = None
  && b.(0).(col-1) <> None then true
  else if is_valid_merge_vertical b (col-1) 0 1
  || is_valid_merge_vertical b (col-1) 1 2
  || is_valid_merge_vertical b (col-1) 2 3
  then true else is_valid_move_down b row (col-1)

(*Returns boolean indicating whether move [m] would alter board [b]*)
let is_valid_move m b =
  match m with
  | Left -> is_valid_move_left b (Array.length b) (Array.length b)
  | Right -> is_valid_move_right b (Array.length b) (Array.length b)
  | Up -> is_valid_move_up b (Array.length b) (Array.length b)
  | Down -> is_valid_move_down b (Array.length b) (Array.length b)
  | (Regular|Evil|Null) -> true

(* checks to see if the 2048 tile has been created yet *)
let check_winning_board (b : board) =
  let win = ref false in
  for i = 0 to (Array.length b) - 1 do
    if Array.exists check_2048_square b.(i) then win := true
  done;
  !win

(* [random_nth_list l] returns a random member of list [l] *)
let random_nth_list l  =
  let len = List.length l in
  List.nth l (Random.int len)
let (>>=) l f = List.concat (List.map f l)
let list_index = [0;1;2;3]

(* [not_avail_squares b] Returns a list of non-empty squares in [b] *)
let not_avail_squares b =
  let all_indicies =
    list_index >>= fun i ->
    list_index >>= fun j ->
    [(i, j)]
  in
  List.filter (fun (i, j) -> b.(i).(j) <> None) all_indicies

(* [empty_squares b] Returns a list of empty squares in [b] *)
let empty_squares b =
  let all_indicies =
    list_index >>= fun i ->
    list_index >>= fun j ->
    [(i, j)]
  in
  List.filter (fun (i, j) -> b.(i).(j) = None) all_indicies

(* [random_avail b] Returns a tuple (i,j) of a random open position
 * in [b] in row i, column j.
 * Precondition: [b] has at least one open position. *)
let random_avail b =
  let avail = empty_squares b in
  random_nth_list avail

(* Inspiration taken from: http://langref.org/ocaml/
 * numbers/mathematical/distance-between-points
 * [distance a b] finds distance between points a b *)
let distance a b =
  sqrt((float(fst a) -. float(fst b))**2.
    +. (float(snd a) -. float(snd b))**2.)

(* [find_max_sq b] Given list of current squares, find max
 * returns (i,j) of the largest square *)
let find_max_sq (b:board) =
  let lst = not_avail_squares b in
  let max_ref = ref 0 in
  let max_pos = ref (0,0) in
  for i = 0 to (List.length lst - 1) do
    let pos = List.nth lst i in
    let sq_val = Gamelogic.square_value b.(fst pos).(snd pos) in
    if sq_val > !max_ref then begin
      max_ref := sq_val;
      max_pos := pos;
    end
    else ()
  done;
  !max_pos

(* [insert_evil_square b] places new square in the worst possible
 * position. Determined by max square and relative distance to that
 * square *)
let insert_evil_square (b:board) =
  let max_pos = find_max_sq b in
  let empty_sq = empty_squares b in
  let min_ref = ref 10. in
  let min_pos = ref (0,0) in
  for i = 0 to (List.length empty_sq - 1) do
    let pos = List.nth empty_sq i in
    let dist = distance max_pos pos in
    if dist < !min_ref then begin
      min_ref := dist;
      min_pos := pos;
    end
    else ()
  done;
  !min_pos

(* [random_sq_value] returns value for new square, with 90% probability
 * of 2, 10% for 4 *)
let random_sq_value () =
  let prob = Random.int 10 in
  if prob = 0 then (Some 4) else (Some 2)

(* [insert_square b evil] inserts pre-determined square [sq] into board [b];
 * checks if in evil mode *)
let insert_square (b : board) evil : unit =
  (* let (i, j) = random_avail b in *)
  let (i, j) =
    if !evil then insert_evil_square b
    else random_avail b in
  let sq = random_sq_value () in
  b.(i).(j) <- sq

(* [check_end_game b] checks end game condition; no more valid moves *)
let check_end_game (b : board) =
  not (is_valid_move Left b || is_valid_move Right b ||
  is_valid_move Up b || is_valid_move Down b)

(* [key_press m b s evil] checks for wining board; inserts new square
 * otherwise *)
let key_press m b s evil =
  if is_valid_move m b then (move m b s;
  if check_winning_board b then () else insert_square b evil)
  else ()
