open Types

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

(* val is_valid_merge_horizontal : board -> int -> int -> int -> bool*)

(** [is_valid_move move board] is [true] if shifting [board] in the direction
    [move] results in a change in the game board. *)
val is_valid_move : move -> board -> bool

(* val is_empty_row : board -> int -> int -> bool *)

(** [insert_square square board] is [board] with [square] inserted
    in a random empty spot. *)
val insert_square : board -> bool ref -> unit

val empty_squares : board -> (int * int) list

val find_max_sq : board -> int * int

val corner_ai: board -> move

(* type staticState = { e : bool; score : score; board : board; } *)
val get_greedy_move: staticState -> move


