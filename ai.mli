open Types

val corner_ai: board -> move

(* type staticState = { e : bool; score : score; board : board; } *)
val get_greedy_move: staticState -> move


