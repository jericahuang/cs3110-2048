(* Types *)

(** The type for game moves corresponding to arrow keys (left, right, up, down). *)
type move = L | R | U | D

(** The type for squares. *)
type square = int option

(** The type for board rows. *)
type row = square list

(** The type for boards. *)
type board = row list



(* Square Values *)

(** [empty] is an empty square. *)
val empty : square

(** [t2] is a square tile with value 2. *)
val t2 : square

(** [t4] is a square tile with value 4. *)
val t4 : square

(** [t8] is a square tile with value 8. *)
val t8 : square

(** [t16] is a square tile with value 16. *)
val t16 : square

(** [t32] is a square tile with value 32. *)
val t32 : square

(** [t64] is a square tile with value 64. *)
val t64 : square

(** [t128] is a square tile with value 128. *)
val t128 : square

(** [t256] is a square tile with value 256. *)
val t256 : square

(** [t512] is a square tile with value 512. *)
val t512 : square

(** [t1024] is a square tile with value 1024. *)
val t1024 : square

(** [t2048] is a square tile with value 2048. *)
val t2048 : square

(** [square_value t] is [t]'s value (if any). *)
val square_value : square -> int option



(* Board and move logic *)

(* Keyup event handler function *)
val keyup : move -> unit

(** [create_board ()] is a new board. *)
val create_board : unit -> board

(** [is_game_over board] is [true] if there are no valid moves. *)
val is_game_over : board -> bool

(** Whether a square is occupied by a winning tile (2048). *)
val is_game_won: square -> bool

(** [is_valid_move move board] is [true] if shifting [board] in the direction
    [move] results in a change in the game board. *)
val is_valid_move : move -> board -> bool

(** [insert_square square board] is [board] with [square] inserted
    in an empty spot. *)
val insert_square : square -> board -> board


