type action = Move of Gameplay.move | Invalid
(** Type of game action. *)

val action : unit -> action
(** [get_action] waits and return the next action from user input. *)