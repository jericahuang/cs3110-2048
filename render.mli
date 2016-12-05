(* This module will handle conversion from ocaml to javascript and
 * drawing capabilities. *)

(******************************************************************************)
(** Render Value                                                             **)
(******************************************************************************)
(* [border] pixel width of border *)
val border : int

(* [sq_w] pixel width of square *)
val sq_w : int

(* [sq_h] pixel height of square *)
val sq_h : int

(* [sq_space] pixel width between squares *)
val sq_space : int

(* [wind_w] pixel width of window *)
val wind_w : int

(* [wind_h] pixel height of window *)
val wind_h : int

(* [board_lines_color] pixel height of window *)
val board_lines_color : int * int * int

(******************************************************************************)
(** Render Functions                                                         **)
(******************************************************************************)

(* [square_dim i j] returns dimentions of square at (i,j) *)
val square_dim : int -> int -> (int * int * int * int)

(* [map_sq_colors val] maps a sqaure to its color value, text color *)
val map_sq_colors : int -> (int * int * int) * (int * int * int)

(* [map_text_size v] maps a sqaure to its text size, y-coord position *)
val map_text_size : int -> int * float

(* [end_game ctx] handles endgame drawing in [ctx] *)
val end_game : Dom_html.canvasRenderingContext2D Js.t -> unit

(* [win_game ctx] handles win game drawing in [ctx] *)
val win_game : Dom_html.canvasRenderingContext2D Js.t -> unit

(* [convert_color (r,g,b)] formats into JS readable rgb value *)
val convert_color : (int * int * int) -> Js.js_string Js.t

(* [draw_empty_sq ctx i j] draws an empty square at (i,j) in [ctx] *)
val draw_empty_sq : Dom_html.canvasRenderingContext2D Js.t -> int -> int -> unit

(* [draw_sq ctx i j sq_v] draws an square with value [sq_v] at (i,j) in [ctx] *)
val draw_sq : Dom_html.canvasRenderingContext2D Js.t -> int -> int -> int -> unit

(* [draw_board ctx b] draws an board [b] in [ctx] *)
val draw_board : Dom_html.canvasRenderingContext2D Js.t -> Types.board -> unit

(* [append_text elm str] adds text [str] to [elm] *)
val append_text : #Dom.node Js.t -> string -> unit

(* Inspiration was taken from js_of_ocaml library:
 * https://github.com/ocsigen/js_of_ocaml/blob/master/
 * examples/boulderdash/boulderdash.ml *)
(* [replace_child parent node] replaces first child
 * of the [parent] with [node] *)
val replace_child : #Dom.node Js.t -> #Dom.node Js.t -> unit

(* [regular_handler ctx evil] changes and draws mode to Regular *)
val regular_handler : Dom_html.canvasRenderingContext2D Js.t -> bool ref -> unit

(* [evil_handler ctx evil] changes and draws mode to Evil *)
val evil_handler : Dom_html.canvasRenderingContext2D Js.t -> bool ref -> unit

(* [change_score score_sp s] updates score [s] in context [score_sp] *)
val change_score : #Dom.node Js.t -> int ref -> unit






