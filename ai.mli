(* This module will handle the AI auto-run algorithms
 * for optimizing game play: corner, greedy, and random. *)

open Types

(* [corner_ai b] uses the strategy of building up the tiles in the
 * corner. It moves all the tiles to the bottom-right. *)
val corner_ai: board -> move

(* [get_greedy_move st] gets the greedy move for the current static state *)
val get_greedy_move: staticState -> move

(* [random_ai b] makes random moves *)
val random_ai: board -> move


