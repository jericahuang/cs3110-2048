(* This module will handle the AI auto-run algorithms
 * for optimizing game play: corner, greedy, and random. *)

open Types

val corner_ai: board -> move

(* type staticState = { e : bool; score : score; board : board; } *)
val get_greedy_move: staticState -> move

val random_ai: board -> move


