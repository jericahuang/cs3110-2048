(* Types *)

exception End_game
exception Win_game

(** The type for game moves corresponding to arrow keys (left, right, up, down). *)
type move =
  | Regular
  | Evil
  | Left
  | Right
  | Up
  | Down

(** The type for squares. *)
type square = int option

(* * The type for board rows.
type row = square list *)

(** The type for boards. *)
type board = square array array

type score = int ref

type state = {
  evil: bool ref;
  s: score;
  b: board;
}

(** [square_value t] is [t]'s value (if any). *)
val square_value : square -> int

val is_empty_square : square -> bool

(* Board and move logic *)

(* Keyup event handler function *)
val key_press : move -> board -> score -> bool ref -> unit

(** [create_board ()] is a new board. Defaults to 4. *)
val init_board : int -> state

(** [is_game_over board] is [true] if there are no valid moves. *)
val check_end_game : board -> bool

(** Whether a square is occupied by a winning tile (2048). *)
val check_2048_square: square -> bool

val check_winning_board: board -> bool

(** [is_valid_move move board] is [true] if shifting [board] in the direction
    [move] results in a change in the game board. *)
val is_valid_move : move -> board -> bool

(** [insert_square square board] is [board] with [square] inserted
    in a random empty spot. *)
val insert_square : board -> bool ref -> unit

val move : move -> board -> score -> unit

val empty_squares : board -> (int * int) list

val find_max_sq : board -> int * int


