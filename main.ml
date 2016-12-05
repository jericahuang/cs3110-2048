open Gameplay
open Gamelogic
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
 EVENT LOOP
*****************************************************************************
*)

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
  | 77 -> Some (Random)
  | _ -> None

(* [play_game ctx score_sp] begins gameplay by
 * init state and drawing the game *)
let rec play_game ctx score_sp =
  let state = init_board 4 in
  Render.draw_board ctx (state.b);
  key_action ctx state.b state.s score_sp state.evil

(* [key_action ctx b s score_sp evil] maps key actions to correct behacior
 * Note: [score_sp] element associated with "score" id *)
and key_action ctx b s score_sp evil =
   H.document##onkeydown <- H.handler (fun e ->
   begin match parse_ev e with
   | Some (Regular) -> Render.regular_handler ctx evil
   | Some (Evil) -> Render.evil_handler ctx evil
   | Some (Move x) -> key_press x b s evil; Render.draw_board ctx b;
                  Render.change_score score_sp s;
   	       			  if check_winning_board b then Render.win_game ctx else
   	       			  if check_end_game b then Render.end_game ctx else ()
   | Some (New) -> Render.replace_child score_sp
                  (document##createTextNode (js("0")));
                  play_game ctx score_sp
   | Some (Corner) -> key_press (corner_ai b) b s evil;
                  Render.draw_board ctx b; Render.change_score score_sp s;
                      Render.change_score score_sp s;
   	       			  if check_winning_board b then Render.win_game ctx else
   	       			  if check_end_game b then Render.end_game ctx else ()
   | Some (Greedy) ->
      let st = {e = !evil; score = s; board = b} in
        key_press (get_greedy_move st) b s evil;
        Render.draw_board ctx b; Render.change_score score_sp s;
        if check_winning_board b then Render.win_game ctx else
        if check_end_game b then Render.end_game ctx else ()
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
  canvas##width <- (Render.wind_w);
  canvas##height <- (Render.wind_h);
  Dom.appendChild game canvas;
  let ctx = canvas##getContext (H._2d_) in
  let score_sp =
    Js.Opt.get (H.document##getElementById(js"score"))
      (fun () -> assert false)
  in
  Render.append_text score_sp "0";
  play_game ctx score_sp

let _ = main ()