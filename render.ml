open Gameplay
(* module Html = Dom_html *)

(* let js = Js.string *)
(* let document = Dom_html.window##.document *)

(* Tile colors *)
let c1 = 0xEDEAEAl
let c2 = 0xF5CCBEl
let c3 = 0xE7A19El
let c4 = 0xBD8996l
let c5 = 0x98829Al

let redof x = (Int32.to_int x land 0xff0000) lsr 16
let greenof x = (Int32.to_int x land 0xff00) lsr 8
let blueof x = (Int32.to_int x land 0xff)

let fontcolor = (redof c1, greenof c1, blueof c1)

let bgcolor = c1
let emptycolor = c2
(* let tilecolor = function
  | T2 | T4 | T8 | T16 -> c3
  | T32 | T64 | T128 -> c4
  | T256 | T512 | T1024 | T2048 -> c5 *)
(*
let draw_empty ctx i j =
  let (x, y, w, h) = Lookandfeel.rect_at i j in
  ctx##fillStyle <- color Lookandfeel.emptycolor;
  ctx##fillRect (float x, float y, float w, float h) *)

let main () =
  let canvas = Dom_html.(createCanvas document) in
  let id2048 = Dom_html.getElementById "2048" in
  Dom.appendChild id2048 canvas

let _ = main ()