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
let document = H.document

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
  (y, x, sq_w, sq_h)

(*
*****************************************************************************
 COLOR CONSTANTS
*****************************************************************************
*)

let board_lines_color = (187,173,160)

(* [map_sq_colors val] maps a sqaure to its
 * color value, text color, fontsize *)
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

let map_text_size v =
  match v with
  | 0 -> 30
  | 2 -> 30
  | 4 -> 30
  | 8 -> 30
  | 16 -> 30
  | 32 -> 30
  | 64 -> 30
  | 128 -> int_of_float (30. /. 1.2)
  | 256 -> int_of_float (30. /. 1.2)
  | 512 -> int_of_float (30. /. 1.2)
  | 1024 -> int_of_float (30. /. 1.5)
  | 2048 -> int_of_float (30. /. 1.5)
  | _ -> failwith "Invalid Tile"

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
(*
let draw_score name =
  let res = document##createDocumentFragment in
  Dom.appendChild res (document##createTextNode (js name)) *)
(* https://github.com/ocsigen/js_of_ocaml/blob/master/examples/boulderdash/boulderdash.ml *)
let replace_child parent node =
  Js.Opt.iter (parent##firstChild) (fun child -> Dom.removeChild parent child);
  Dom.appendChild parent node

let append_text e str =
  Dom.appendChild e (document##createTextNode (js str))

let draw_empty_sq ctx i j =
  let (x, y, w, h) = square_dim i j in
  let empty_color = fst (map_sq_colors 0) in
  ctx##fillStyle <- convert_color empty_color;
  ctx##fillRect (float x, float y, float w, float h)

let draw_sq ctx i j sq_v =
  let (x, y, w, h) = square_dim i j in
  let sq_colors = map_sq_colors sq_v in
  let sq_val_str = string_of_int sq_v in
  ctx##fillStyle <- convert_color (fst sq_colors);
  ctx##fillRect (float x, float y, float w, float h);
  (* ctx##font <- js(Printf.sprintf "%dpx Verdana" ); *)
  ctx##font <- js(Printf.sprintf "%dpx Verdana" (map_text_size sq_v));
  ctx##textAlign <- js("center");
  ctx##fillStyle <- convert_color (snd sq_colors);
  ctx##fillText (js(sq_val_str),
                float x +. float sq_w /. 2.,
                float y +. float sq_h /. 1.5)

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

let parse_ev e =
  match e##keyCode with
  | 37 -> Some (Left)
  | 38 -> Some (Up)
  | 39 -> Some (Right)
  | 40 -> Some (Down)
  | _ -> None

(* score_sp: element associated with "score" id *)
let key_action ctx b s score_sp =
   H.document##onkeydown <- H.handler (fun e ->
   begin match parse_ev e with
   | Some (Left) -> key_press Left (b,s)
   | Some (Up) -> key_press Up (b,s)
   | Some (Right) -> key_press Right (b,s)
   | Some (Down) -> key_press Down (b,s)
   | _ -> ()
   end; draw_board ctx b;
   let txt = document##createTextNode (js(string_of_int !s)) in
   replace_child score_sp txt;
   Js._true)

let rec play_game ctx score_sp =
  let (b,s) = init_board 4 in
  draw_board ctx b;
  key_action ctx b s score_sp
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
  let score_sp =
    Js.Opt.get (H.document##getElementById(js"score"))
      (fun () -> assert false)
  in
  append_text score_sp "0";
  play_game ctx score_sp

let _ = main ()