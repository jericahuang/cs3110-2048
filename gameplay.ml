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

let init_board size =
  if size < 1 then failwith "Invalid matrix size"
  else
    let b = Array.make_matrix 4 4 {value = None} in
    b.(3).(3) <- {value = (Some 2)}; b

(* [check_2048_sqaure s] che *)
let check_2048_sqaure (s : square) =

