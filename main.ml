open Gameplay
open Gamelogic
open Render
open Types

(*
*****************************************************************************
 MODULE DECLARATIONS
*****************************************************************************
*)

module H = Dom_html

let js = Js.string
let document = H.document

(*
*****************************************************************************
 BOARD AND SQUARE DIMENSIONS
*****************************************************************************
*)

let border = 25
let sq_w = 180
let sq_h = 180
let sq_space = 10
let wind_w = (3 * border) + (2 * border) + (4 * sq_w)
let wind_h = (3 * border) + (2 * border) + (4 * sq_h)

let square_dim i j =
  let x = i * (sq_w + border) + border in
  let y = j * (sq_h + border) + border in
  (y, x, sq_w, sq_h)

(*
*****************************************************************************
 COLOR CONSTANTS
*****************************************************************************
*)

let board_lines_color = (187,173,160)

(* [map_sq_colors val] maps a sqaure to its color value, text color *)
let map_sq_colors v =
  let white_text = (255, 252, 245) in
  let brown_text = (119, 110, 101) in
  match v with
  | 0 -> (205,193,180), brown_text
  | 2 -> (238,228,218), brown_text
  | 4 -> (237,224,200), brown_text
  | 8 -> (242,177,121), white_text
  | 16 -> (245,149,99), white_text
  | 32 -> (246,124,95), white_text
  | 64 -> (246,93,59), white_text
  | 128 -> (234,204,112), white_text
  | 256 -> (233,201,97), white_text
  | 512 -> (232,197,89), white_text
  | 1024 -> (237,197,63), white_text
  | 2048 -> (238,194,46), white_text
  | _ -> failwith "Invalid Tile"

(* Gives back text size and Y divider *)
(* let map_text_size v =
  match v with
  | 0 -> 38
  | 2 -> 38
  | 4 -> 38
  | 8 -> 38
  | 16 -> 35
  | 32 -> 35
  | 64 -> 35
  | 128 -> int_of_float (38. /. 1.2)
  | 256 -> int_of_float (38. /. 1.2)
  | 512 -> int_of_float (38. /. 1.2)
  | 1024 -> int_of_float (38. /. 1.5)
  | 2048 -> int_of_float (38. /. 1.5)
  | _ -> failwith "Invalid Tile" *)

(* [map_text_size v] maps a sqaure to its text size, y-coord position *)
let map_text_size v =
  match v with
  | 0 -> 75, 1.5
  | 2 -> 75, 1.5
  | 4 -> 75, 1.5
  | 8 -> 75, 1.5
  | 16 -> 72, 1.5
  | 32 -> 72, 1.5
  | 64 -> 72, 1.5
  | 128 -> int_of_float (75. /. 1.2), 1.55
  | 256 -> int_of_float (75. /. 1.2), 1.55
  | 512 -> int_of_float (75. /. 1.2), 1.55
  | 1024 -> int_of_float (75. /. 1.5), 1.55
  | 2048 -> int_of_float (75. /. 1.5), 1.55
  | _ -> failwith "Invalid Tile"

(*
*****************************************************************************
 STRING CONVERSIONS
*****************************************************************************
*)

(* [convert_color (r,g,b)] formats into JS readable rgb value *)
let convert_color (r,g,b) =
  js(Printf.sprintf "rgb(%d,%d,%d)" r g b)

(*
*****************************************************************************
 DRAW FUNCTIONS
*****************************************************************************
*)

(* https://github.com/ocsigen/js_of_ocaml/blob/master/examples/boulderdash/boulderdash.ml *)
(* [replace_child parent node] replaces first child
 * of the [parent] with [node] *)
let replace_child parent node =
  Js.Opt.iter (parent##firstChild) (fun child -> Dom.removeChild parent child);
  Dom.appendChild parent node

(* [append_text elm str] adds text [str] to [elm] *)
let append_text elm str =
  Dom.appendChild elm (document##createTextNode (js str))

(* [draw_empty_sq ctx i j] draws an empty square at (i,j) in [ctx] *)
let draw_empty_sq ctx i j =
  let (x, y, w, h) = square_dim i j in
  let empty_color = fst (map_sq_colors 0) in
  ctx##fillStyle <- convert_color empty_color;
  ctx##fillRect (float x, float y, float w, float h)

(* [draw_sq ctx i j sq_v] draws an square with value [sq_v] at (i,j) in [ctx] *)
let draw_sq ctx i j sq_v =
  let (x, y, w, h) = square_dim i j in
  let sq_colors = map_sq_colors sq_v in
  let sq_val_str = string_of_int sq_v in
  let text_vals = map_text_size sq_v in
  ctx##fillStyle <- convert_color (fst sq_colors);
  ctx##fillRect (float x, float y, float w, float h);
  (* ctx##font <- js(Printf.sprintf "%dpx Verdana" ); *)
  ctx##font <- js(Printf.sprintf "700 %dpx Clearsans, Arial" (fst text_vals));
  ctx##textAlign <- js("center");
  ctx##fillStyle <- convert_color (snd sq_colors);
  ctx##fillText (js(sq_val_str),
                float x +. float sq_w /. 2.05,
                float y +. float sq_h /. (snd text_vals) )

(* [draw_board ctx b] draws an board [b] in [ctx] *)
let draw_board ctx b =
  ctx##fillStyle <- (convert_color board_lines_color);
  ctx##fillRect (0.0, 0.0, float wind_w, float wind_h);
  for i = 0 to 3 do
    for j = 0 to 3 do
      (match b.(i).(j) with
      | None -> draw_empty_sq ctx i j
      | (Some v) -> draw_sq ctx i j v)
    done
  done

(*
*****************************************************************************
 EVENT LOOP
*****************************************************************************
*)

(* key defines the types of actions within the game *)
type key =
	| Move of move
	| Regular
	| Evil
	| New
  | Greedy
  | Corner

(* [end_game ctx] handles endgame drawing in [ctx] *)
let end_game ctx =
	ctx##fillStyle <- convert_color (246,93,59);
	ctx##fillRect (0.0, 0.0, float wind_w, float wind_h);
	ctx##font <- js("700 %dpx Clearsans, Arial");
	ctx##textAlign <- js("center");
	ctx##fillStyle <- convert_color (255, 252, 245);
	ctx##fillText (js"You Lose! :(", float wind_w /. 2., float wind_h /. 2.)

(* [win_game ctx] handles win game drawing in [ctx] *)
let win_game ctx =
	ctx##fillStyle <- convert_color (0,230,0);
	ctx##fillRect (0.0, 0.0, float wind_w, float wind_h);
	ctx##font <- js("700 %dpx Clearsans, Arial");
	ctx##textAlign <- js("center");
	ctx##fillStyle <- convert_color (255, 252, 245);
	ctx##fillText (js"You Win! :)", float wind_w /. 2., float wind_h /. 2.)

(* [parse_ev e] parses key presses to correct behavior *)
let parse_ev e =
  match e##keyCode with
  | 37 -> Some (Move Left)
  | 38 -> Some (Move Up)
  | 39 -> Some (Move Right)
  | 40 -> Some (Move Down)
  | 82 -> Some (Regular)
  | 78 -> Some (New)
  | 69 -> Some (Evil)
  | 67 -> Some (Corner)
  | 71 -> Some (Greedy)
  | _ -> None

(* [regular_handler ctx evil] changes and draws mode to Regular *)
let regular_handler ctx evil =
  let mode =
    Js.Opt.get (H.document##getElementById(js"mode"))
      (fun () -> assert false)
  in
  let txt = document##createTextNode (js("Regular")) in
  replace_child mode txt;
  evil := false

(* [regular_handler ctx evil] changes and draws mode to Evil *)
let evil_handler ctx evil =
  let mode =
    Js.Opt.get (H.document##getElementById(js"mode"))
      (fun () -> assert false)
  in
  let txt = document##createTextNode (js("Evil")) in
  replace_child mode txt;
  evil := true

(* [change_score score_sp s] updates score [s] in context [score_sp] *)
let change_score score_sp s =
   let txt = document##createTextNode (js(string_of_int !s)) in
   replace_child score_sp txt

(* [play_game ctx score_sp] begins gameplay by
 * init state and drawing the game *)
let rec play_game ctx score_sp =
  let state = init_board 4 in
  draw_board ctx (state.b);
  key_action ctx state.b state.s score_sp state.evil

(* [key_action ctx b s score_sp evil] maps key actions to correct behacior
 * Note: [score_sp] element associated with "score" id *)
and key_action ctx b s score_sp evil =
   H.document##onkeydown <- H.handler (fun e ->
   begin match parse_ev e with
   | Some (Regular) -> (regular_handler ctx evil)
   | Some (Evil) -> (evil_handler ctx evil)
   | Some (Move x) -> key_press x b s evil; draw_board ctx b; change_score score_sp s;
   	       			  if check_winning_board b then win_game ctx else
   	       			  if check_end_game b then end_game ctx else ()
   | Some (New) -> replace_child score_sp (document##createTextNode (js("0"))); play_game ctx score_sp
   | Some (Corner) -> key_press (corner_ai b) b s evil; draw_board ctx b; change_score score_sp s;
                  if check_winning_board b then win_game ctx else
                  if check_end_game b then end_game ctx else ()
   | Some (Greedy) -> (*key_press (get_greedy_move {e=!evil;score=s;board=b}) s evil; draw_board ctx b; change_score score_sp s;
                  if check_winning_board b then win_game ctx else
                  if check_end_game b then end_game ctx else*) ()
   | None -> ()
   end;
   Js._true)

(* [main] initially sets and draws the game and triggers gameplay *)
let main () =
  let game =
    Js.Opt.get (H.document##getElementById(js"2048"))
      (fun () -> assert false)
  in
  let canvas = H.createCanvas H.document in
  (* canvas##width <- (wind_w + 250);
  canvas##height <- (wind_h + 250); *)
(*   canvas##style##width <- js(string_of_int (wind_w - 100));
  canvas##style##height <- js(string_of_int (wind_h - 100)); *)
  canvas##width <- (wind_w);
  canvas##height <- (wind_h);
  Dom.appendChild game canvas;
  let ctx = canvas##getContext (H._2d_) in
  let score_sp =
    Js.Opt.get (H.document##getElementById(js"score"))
      (fun () -> assert false)
  in
  append_text score_sp "0";
  play_game ctx score_sp

let _ = main ()