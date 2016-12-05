(* This module will delegate move input, check for valid moves,
 * and handle game ending, inserting random squares, and AI. *)

open Types

(*******************************************************
   ____    _    __  __ _____   ____  _        _ __   __
  / ___|  / \  |  \/  | ____| |  _ \| |      / \\ \ / /
 | |  _  / _ \ | |\/| |  _|   | |_) | |     / _ \\ V / 
 | |_| |/ ___ \| |  | | |___  |  __/| |___ / ___ \| |  
  \____/_/   \_\_|  |_|_____| |_|   |_____/_/   \_\_|  

********************************************************)


(* [is_empty_square s] checks if [s] is an empty square. *)
val is_empty_square : square -> bool

(* [empty_squares b] Returns a list of empty squares in [b] *)
val empty_squares : board -> (int * int) list

(* [find_max_sq b] Given list of current squares, find max
 * returns (i,j) of the largest square *)
val find_max_sq : board -> int * int

(* [init_board ()] is a new board. Defaults to 4. *)
val init_board : unit -> state

(* [check_2048_square s] checks whether a square 
 * is occupied by a winning tile (2048). *)
val check_2048_square: square -> bool

(* [is_empty_col b col size] returns whether or not column [col] 
 * is empty in board [b] of length/width [size] *)
val is_empty_col : board -> int -> int -> bool

(* [is_valid_move m b] is [true] if shifting [board] in the direction
 * [move] results in a change in the game board. *)
val is_valid_move : move -> board -> bool

(* [check_winning_board b] is [true] if the 2048 tile has been created *)
val check_winning_board: board -> bool

(* [insert_evil_square b] places new square in the worst possible
 * position. Determined by max square and relative distance to that
 * square *)
val insert_evil_square : board -> int * int

(* [insert_square b evil] is [board] with [square] inserted
 * in a random empty spot. *)
val insert_square : board -> bool ref -> unit

(* [check_end_game board] is [true] if there are no valid moves. *)
val check_end_game : board -> bool

(* [key_press m b s evil] handles the keyboard inputs. *)
val key_press : move -> board -> score -> bool ref -> unit