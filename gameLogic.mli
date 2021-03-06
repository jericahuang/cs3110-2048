(* This module will handle return the updated board as a matrix.
 * Note, this includes combinations and replacement. *)

open Types

(*********************************************************
   ____    _    __  __ _____   _     ___   ____ ___ ____ 
  / ___|  / \  |  \/  | ____| | |   / _ \ / ___|_ _/ ___|
 | |  _  / _ \ | |\/| |  _|   | |  | | | | |  _ | | |    
 | |_| |/ ___ \| |  | | |___  | |__| |_| | |_| || | |___ 
  \____/_/   \_\_|  |_|_____| |_____\___/ \____|___\____|

*********************************************************)
                                                         

(* [square_value t] is [t]'s value (if any). *)
val square_value : square -> int

(* [combine_tiles c r f] replaces the [r][c] element with
 * what is returned from applying f to the [r][c] element. Note [r] is a
 * list while [c] is an int that represent the index within the list *)
val combine_tiles : board -> int ref -> int -> int -> int -> unit

(* [shift_left b row s1 s2] shifts two tiles [s1] and [s2] to the left and
 * accounts for new open space generated by the shift *)
val shift_left : board -> int -> int -> int -> unit

(* [fix_row b row size] will remove all of the blanks of a [row] to make merging
 * easier when a move is made on the board *)
val fix_row : board -> int -> int -> unit

(* [is_empty_row b row size] returns whether or not row [r] is empty in board [b] of
 * length/width [size] *)
val is_empty_row : board -> int -> int -> bool

(* [is_valid_merge_horizontal b row s1 s2] returns a bool that
 * indicates whether or not a valid left or right
 * move is valid given two squares [s1] and [s2] and a [row] *)
val is_valid_merge_horizontal : board -> int -> int -> int -> bool

(* [is_valid_merge_vertical b col s1 s2] returns a bool that
 * indicates whether or not a valid up or down
 * move is valid given two squares [s1] and [s2] and a [col] *)
val is_valid_merge_vertical : board -> int -> int -> int -> bool

(* [process_row b s row col size] will look through a [row] and look for a valid merge
 * and if there is one it will combine the tiles together *)
val process_row : board -> int ref -> int -> int -> int -> unit

(* [move_left b s row col size] works to complete all actions of a move (shifting
 * and merging tiles and incrementing the score accordingly) in the leftward direction
 * as long as the row is not empty *)
val move_left : board -> int ref -> int -> 'a -> int -> unit

(* [to_lst b size] converts the board [b] to a matrix list *)
val to_lst : 'a array array -> int -> 'a list list

(* [to_lst_rev b size] converts the board to a matrix list
 * but each row is in reverse order *)
val to_lst_rev : 'a array array -> int -> 'a list list

(* [to_arr lst arr size] converts the matrix list [lst] back into
 * a valid board array *)
val to_arr : 'a list list -> 'a array array -> int -> unit

(* [get_head lst] gets the first value of a row *)
val get_head : 'a list list -> 'a list

(* [get_tail lst] gets the rest of the row excluding the first value *)
val get_tail : 'a list list -> 'a list list

(* [rotate lst] rotates the board so that move_left can be applied to
 * all moves using the same function *)
val rotate : 'a list list -> 'a list list

(* [rotate_up b] rotates the board 90 degrees counter-clockwise *)
val rotate_up : 'a array array -> unit

(* [rotate_right b] rotates the board to be in reverse order *)
val rotate_right : 'a array array -> unit

(* [move m b s] moves the board [b] in direction [m] *)
val move : move -> board -> score -> unit