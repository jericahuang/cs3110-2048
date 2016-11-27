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

(* [init_board size] initializes the board with [size].
 * Starts with square of 2 in bottom left corner.
 * requires: size >= 1 *)
let init_board size =
  if size < 1 then failwith "Invalid matrix size"
  else
    let b = Array.make_matrix 4 4 {value = None} in
    b.(3).(3) <- {value = (Some 2)}; b

(* [check_2048_sqaure s] returns if 2048 square has been formed. *)
let check_2048_square (s : square) =
  square_value s.value = 2048

let check_winning_board (b : board) =
  let win = ref false in
  for i = 0 to (Array.length b) - 1 do
    if Array.exists check_2048_square b.(i) then win := true
  done;
  !win
