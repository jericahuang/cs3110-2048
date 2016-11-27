(** [board_to_image board] is an image from [board]. *)
val board_to_image : Gameplay.board -> unit

(** [animate_board move board] is the image of the animation of the 
  	move [move] upon board [board]. Also overlays game win/loss message*)
val animate_board : Gameplay.move -> Gameplay.board -> unit
