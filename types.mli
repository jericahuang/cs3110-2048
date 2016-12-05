(* [move] defines the types of moves within the game *)
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

(* [square] defines a square in the board *)
type square = int option

(* [board] matrix of squares *)
type board = square array array

(* [score] current score in the game *)
type score = int ref

(* [state] is the current state of the game including
 * evil: whether in Evil or Regular mode
 * s: current score
 * b: current board *)
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