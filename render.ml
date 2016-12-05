(* open Gameplay *)
module H = Dom_html

let js = Js.string
let document = Dom_html.window##document

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

(* [square_dim i j] returns dimentions of square at (i,j) *)
let square_dim i j =
  let x = i * (sq_w + border) + border in
  let y = j * (sq_h + border) + border in
  (y, x, sq_w, sq_h)

(*
*****************************************************************************
 COLOR CONSTANTS
*****************************************************************************
*)
(* Color of board lines *)
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

(*
*****************************************************************************
 STATE AND SCORE FUNCTIONS
*****************************************************************************
*)

(* [regular_handler ctx evil] changes and draws mode to Regular *)
let regular_handler ctx evil =
  let mode =
    Js.Opt.get (H.document##getElementById(js"mode"))
      (fun () -> assert false)
  in
  let txt = document##createTextNode (js("Regular")) in
  replace_child mode txt;
  evil := false

(* [evil_handler ctx evil] changes and draws mode to Evil *)
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

let main () =
  let canvas = Dom_html.(createCanvas document) in
  let id2048 = Dom_html.getElementById "2048" in
  Dom.appendChild id2048 canvas

let _ = main ()









