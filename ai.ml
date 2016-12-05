open Types
open Gameplay
open Gamelogic

(***********************************************
   ____                                _    ___ 
  / ___|___  _ __ _ __   ___ _ __     / \  |_ _|
 | |   / _ \| '__| '_ \ / _ \ '__|   / _ \  | | 
 | |__| (_) | |  | | | |  __/ |     / ___ \ | | 
  \____\___/|_|  |_| |_|\___|_|    /_/   \_\___|

************************************************)
                                                
(* [corner_ai b] uses the strategy of building up the tiles in the
 * corner. It moves all the tiles to the bottom-right. *)
let corner_ai (b : board) : move =
  if is_valid_move Right b then Right else
  if is_valid_move Down b then Down else
  if is_valid_move Left b then Left else Up

(************************************************
   ____                   _            _    ___ 
  / ___|_ __ ___  ___  __| |_   _     / \  |_ _|
 | |  _| '__/ _ \/ _ \/ _` | | | |   / _ \  | | 
 | |_| | | |  __/  __/ (_| | |_| |  / ___ \ | | 
  \____|_|  \___|\___|\__,_|\__, | /_/   \_\___|
                            |___/               
 ************************************************)

let moveList = [Up;Down;Left;Right;]

(* [compare_first item1 item2] sorts a 2-tup list based on the 1st tup value*)
let compare_first (item1 : (score * movePair)) (item2 : (score * movePair)) =
  compare !(fst item1) !(fst item2)

(* [move_result m b s evil] results in state (evil, score, board) resulting 
 * from shifting [b] in [m] direction with a score of [s] and [e] evil state *)
let move_result m b s evil: staticState =
  let copy = Array.make_matrix 4 4 None in
  let new_score = ref !s in
  to_arr (to_lst b 0) copy 0;
  move m copy new_score;
  {e = evil; score = new_score; board = copy}

(* Sorts a scores_to_moves list [l] from the highest to lowest score*)
let sort_moveList_scores (l : score_to_moves) : score_to_moves =
  List.rev (List.sort compare_first l)

(**
  * [get_greedy_move st] gets the greedy move for the current static state 
  *  The greedy algorithm calculates which series of two moves will result in the
  * highest score, and then proceeds to make the first of those moves. It always 
  * re-evaluates based on the new state, but will often proceed with the second 
  * move it originally planned.
  *)
let get_greedy_move st =
  let score_moves = ref [] in
    let valid_moves_1 =
      List.filter (fun m -> is_valid_move m st.board) moveList in

    for i1=0 to (List.length valid_moves_1 - 1) do
      let move1 = List.nth valid_moves_1 i1 in
      let m1_result = move_result move1 st.board st.score st.e in
      let valid_moves_2 =
        List.filter (fun m -> is_valid_move m m1_result.board) moveList in

      for i2 = 0 to (List.length valid_moves_2 - 1) do
        let move2 = List.nth valid_moves_2 i2 in
        let m1m2_result =
          move_result move2 m1_result.board m1_result.score m1_result.e in
          score_moves := !score_moves@[(m1m2_result.score, (move1, move2))]
      done
    done;
    if List.length !score_moves > 0 then
      fst (snd (List.nth (sort_moveList_scores !score_moves) 0))
    else Null

(******************************************************
  ____                 _                      _    ___ 
 |  _ \ __ _ _ __   __| | ___  _ __ ___      / \  |_ _|
 | |_) / _` | '_ \ / _` |/ _ \| '_ ` _ \    / _ \  | | 
 |  _ < (_| | | | | (_| | (_) | | | | | |  / ___ \ | | 
 |_| \_\__,_|_| |_|\__,_|\___/|_| |_| |_| /_/   \_\___|
                                                       
*******************************************************)

(* [random_ai b] returns a random move *)
let random_ai (b : board) : move =
  let valid_moves =
      List.filter (fun m -> is_valid_move m b) moveList in
  let rand_ind = Random.int (List.length valid_moves) in
  List.nth valid_moves rand_ind


