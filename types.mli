type move =
  | Regular
  | Evil
  | Left
  | Right
  | Up
  | Down
  | Null

(* key defines the types of actions within the game *)
type key =
  | Move of move
  | Regular
  | Evil
  | New
  | Greedy
  | Corner
  | Random

type square = int option

type board = square array array

type score = int ref

type state = {
  evil: bool ref;
  s: score;
  b: board;
}

(* movePair defines a 2-tup of two consecutive moves *)
type movePair = move * move

(*score_to_moves defines a list of score-movePair 2-tups.
 * [score] is the resulting score of moving the two 
 * moves in [movePair] *)
type score_to_moves = (score * movePair) list

(* staticState defines a static state in time, consisting
 * of the current evil boolean flag, score (int ref), and
  * static board. *)
type staticState = {
  e: bool;
  score: score;
  board: board;
}