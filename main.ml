open Gameplay
open Gamelogic
open Render

(*
*****************************************************************************
 MODULE DECLARATIONS
*****************************************************************************
*)

module H = Dom_html

let js = Js.string

(*
*****************************************************************************
 BOARD AND SQUARE DIMENSIONS
*****************************************************************************
*)

let border = 10
let sq_w = 80
let sq_h = 80
let sq_space = 10
let wind_w = (3 * border) + (2 * border) + (4 * sq_w)
let wind_h = (3 * border) + (2 * border) + (4 * sq_h)

let square_dim i j =
  let x = i * (sq_w + border) + border in
  let y = j * (sq_h + border) + border in
  (x, y, sq_w, sq_h)

(*
*****************************************************************************
 COLOR CONSTANTS
*****************************************************************************
*)

let board_lines_color = (73,68,62)

(*
*****************************************************************************
 STRING CONVERSIONS
*****************************************************************************
*)

let convert_color (r,g,b) =
  js(Printf.sprintf "rgb(%d,%d,%d)" r g b)

(*
*****************************************************************************
 DRAW FUNCTIONS
*****************************************************************************
*)

(* [map_tile_colors val] maps a sqaure to its
 * color value, text color, fontsize *)
let map_tile_colors v =
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

let draw_empty_sq ctx i j =
  let (x, y, w, h) = square_dim i j in
  let empty_color = fst (map_tile_colors 0) in
  ctx##fillStyle <- convert_color empty_color;
  ctx##fillRect (float x, float y, float w, float h)

let draw_sq ctx i j sq_v =
  let (x, y, w, h) = square_dim i j in
  let sq_colors = map_tile_colors sq_v in
  let sq_val_str = string_of_int sq_v in
  ctx##fillRect (float x, float y, float w, float h);
  ctx##fillStyle <- convert_color (fst sq_colors);
  (* ctx##fillStyle (js"black"); *)
  ctx##fillText (js(sq_val_str),
                float x +. float sq_w /. 2.0,
                float y +. float sq_h /. 2.0 )

let draw_board ctx b =
  ctx##fillRect (0.0, 0.0, float wind_w, float wind_h);
  ctx##fillStyle <- (convert_color board_lines_color);
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

let rec play_game ctx =
  let b = Gameplay.init_board 4 in
  draw_board ctx b
  (* draw_empty_sq ctx 0 0 *)

let main () =
  let game =
    Js.Opt.get (H.document##getElementById(js"2048"))
      (fun () -> assert false)
  in
  let canvas = H.createCanvas H.document in
  canvas##width <- wind_w;
  canvas##height <- wind_w;
  Dom.appendChild game canvas;
  let ctx = canvas##getContext (H._2d_) in
  play_game ctx

let _ = main ()

let start () = ()