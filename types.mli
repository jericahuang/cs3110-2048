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

type movePair = move * move
type score_to_moves = (score * movePair) list

type staticState = {
  e: bool;
  score: score;
  board: board;
}