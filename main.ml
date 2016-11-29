open Gameplay
open Gamelogic
open Render

(*
let _ = Html.addEventListener Html.document Html.Event.keydown (Html.handler Director.keydown) Js._true in
  let _ = Html.addEventListener Html.document Html.Event.keyup (Html.handler Director.keyup) Js._true in
 *)

module H = Dom_html

let js = Js.string
(* let document = Html.document *)

let redof x = (Int32.to_int x land 0xff0000) lsr 16
let greenof x = (Int32.to_int x land 0xff00) lsr 8
let blueof x = (Int32.to_int x land 0xff)


let color c =
  js(Printf.sprintf "rgb(%d,%d,%d)" (redof c) (greenof c) (blueof c))

let border = 10
let cell_w = 80
let cell_h = 80
let inter_cell = 10
let win_w = 2 * border + 4 * cell_w + 3 * inter_cell
let win_h = 2 * border + 4 * cell_h + 3 * inter_cell

let draw_empty ctx i j =
  let (x, y, w, h) = (10, 10, 80, 80) in
  ctx##fillStyle <- color 0xF5CCBEl;
  ctx##fillRect (float x, float y, float w, float h)

let rec play_game ctx =
  draw_empty ctx 0 0

let main () =
  let game =
    Js.Opt.get (H.document##getElementById(js"2048"))
      (fun () -> assert false)
  in
  let canvas = H.createCanvas H.document in
  canvas##width <- win_w;
  canvas##height <- win_h;
  Dom.appendChild game canvas;
  let ctx = canvas##getContext (H._2d_) in
  play_game ctx

let _ = main ()

let start () = ()