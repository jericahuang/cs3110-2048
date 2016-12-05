
type move =
  | Regular
  | Evil
  | Left
  | Right
  | Up
  | Down
  | Null

type square = int option

type board = square array array

type score = int ref

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