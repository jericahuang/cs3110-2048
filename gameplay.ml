type move = L | R | U |D
type square = int option
type row = square array
type board = row array

let empty = None
let t2 = Some 2
let t4 = Some 4
let t8 = Some 8 
let t16 = Some 16
let t32 = Some 32
let t64 = Some 64
let t128 = Some 128
let t256 = Some 256
let t512 = Some 512
let t1028 = Some 1028
let t2048 = Some 2048

let square_value v = 
	match v with
	| None -> 0
	| Some i -> i
