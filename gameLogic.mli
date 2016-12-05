(* This module will handle return the updated board as a matrix.
 * Note, this includes combinations and replacement. *)

open Types

(******************************************************************************)
(** Game Logic functions                                                     **)
(******************************************************************************)

(** [square_value t] is [t]'s value (if any). *)
val square_value : square -> int

(* [combine_tiles c r f] replaces the [r][c] element with
 * what is returned from applying f to the [r][c] element. Note [r] is a
 * list while [c] is an int that represent the index within the list *)
val combine_tiles : board -> int ref -> int -> int -> int -> unit

(* [replace_tile row f] replaces the first element in [row] with an element.
 * Note, this will be used for random tile insert, hence the new insert will
 * never be None. *)
val replace_tile : board -> int -> int -> int option -> unit

val move_left : board -> int ref -> int -> 'a -> int -> unit 

val move : move -> board -> score -> unit