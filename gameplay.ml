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

let create_board l = 
	Array.make_matrix l l empty

let rec full_board b l = 
	if l = 0 then true else
	if Array.mem empty b.(l-1) then false else full_board b (l-1)
	
let is_game_over b = 
	if full_board b (Array.length b) then 

let winning_board b l = 
	if l = 0 then false else
	if Array.mem t2048 b.(l-1) then true else winning_board b (l-1)

let is_game_won b =
	winning_board b (Array.length b)

	(* remember to ask about array *)