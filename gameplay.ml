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
    (*b.(3).(3) <- Some 1024; b.(3).(2) <- Some 1024;*)
    {
      evil = e;
      s = s;
      b = b;
    }
    (* ([|[|None; Some 1024; Some 64; None|];
    [|None; Some 1024; None; None|];
    [|None; None; None; None|];
    [|Some 2; None; None; Some 8|]|], s) *)

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
  if is_valid_merge_horizontal b (row-1) 0 1 || is_valid_merge_horizontal b (row-1) 1 2
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
  else if is_valid_merge_vertical b (col-1) 0 1 || is_valid_merge_vertical b (col-1) 1 2
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
  else if is_valid_merge_vertical b (col-1) 0 1 || is_valid_merge_vertical b (col-1) 1 2
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

(* Random square insertion *)
(* Returns a random member of list [l] *)
let random_nth_list l  =
  let len = List.length l in
  List.nth l (Random.int len)
let (>>=) l f = List.concat (List.map f l)
let list_index = [0;1;2;3]

(* Repetive code? *)
let not_avail_squares b =
  let all_indicies =
    list_index >>= fun i ->
    list_index >>= fun j ->
    [(i, j)]
  in
  List.filter (fun (i, j) -> b.(i).(j) <> None) all_indicies

(* Repetive code? Returns a list of empty squares *)
let empty_squares b =
  let all_indicies =
    list_index >>= fun i ->
    list_index >>= fun j ->
    [(i, j)]
  in
  List.filter (fun (i, j) -> b.(i).(j) = None) all_indicies

(* Returns a tuple (i,j) of a random open position in [b] in row i,
 * column j. Precondition: [b] has at least one open position.
 *)
let random_avail b =
  let avail = empty_squares b in
  random_nth_list avail

(* http://langref.org/ocaml/numbers/mathematical/distance-between-points *)
let distance a b =
  sqrt((float(fst a) -. float(fst b))**2.
    +. (float(snd a) -. float(snd b))**2.)

(* Given list of current squares, find max
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

let random_sq_value () =
  let prob = Random.int 10 in
  if prob = 0 then (Some 4) else (Some 2)

(* Inserts pre-determined square [sq] into board [b] *)
let insert_square (b : board) evil : unit =
  (* let (i, j) = random_avail b in *)
  let (i, j) =
    if !evil then insert_evil_square b
    else random_avail b in
  let sq = random_sq_value () in
  b.(i).(j) <- sq

(* ASSUMING FUNCTIONALITY use is_valid_move*)
let check_end_game (b : board) =
  not (is_valid_move Left b || is_valid_move Right b ||
  is_valid_move Up b || is_valid_move Down b)

let key_press m b s evil =
  if is_valid_move m b then (move m b s;
  if check_winning_board b then () else insert_square b evil)
  else ()

(* Corner AI *)
let corner_ai b =
  if is_valid_move Right b then Right else
  if is_valid_move Down b then Down else
  if is_valid_move Left b then Left else Up


(* Greedy AI *)

let moveList = [Up;Down;Left;Right;]

(*'compare' function to sort a 2-tup list based on the 1st tup value*)
let compare_first (item1 : (score * movePair)) (item2 : (score * movePair)) =
  compare !(fst item1) !(fst item2)

(*The state (evil, score, board) resulting from shifting
 * [b] in [m] direction with a score of [s] and [e] evil state*)
let move_result m b s evil: staticState =
  let copy = Array.make_matrix 4 4 None in
  let new_score = ref !s in
  to_arr (to_lst b 0) copy 0;
  move m copy new_score;
  {e = evil; score = new_score; board = copy}

(*Sorts a scores_to_moves list [l] from the highest to lowest score*)
let sort_moveList_scores (l : score_to_moves) : score_to_moves =
  List.rev (List.sort compare_first l)

(*Gets the greedy move for the current static state*)
let get_greedy_move (st : staticState) : move =
  let score_moves = ref [] in
    let valid_moves_1 =
      List.filter (fun m -> is_valid_move m st.board) moveList in

    for i1=0 to (List.length valid_moves_1 - 1) do
      let move1 = List.nth valid_moves_1 i1 in
      let m1_result = move_result move1 st.board st.score st.e in
      let valid_moves_2 =
        List.filter (fun m -> is_valid_move m m1_result.board) moveList in

      for i2 = 0 to (List.length valid_moves_2 - 1) do
        let move2 = List.nth valid_moves_2 i2 in
        let m1m2_result =
          move_result move2 m1_result.board m1_result.score m1_result.e in
          score_moves := !score_moves@[(m1m2_result.score, (move1, move2))]
      done
    done;
    if List.length !score_moves > 0 then
      fst (snd (List.nth (sort_moveList_scores !score_moves) 0))
    else Null


