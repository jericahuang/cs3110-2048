open Types
open Gameplay
open Gamelogic

(* Corner AI *)
let corner_ai b =
  if is_valid_move Right b then Right else
  if is_valid_move Down b then Down else
  if is_valid_move Left b then Left else Up


(* Greedy AI *)

let moveList = [Up;Down;Left;Right;]

(*'compare' function to sort a 2-tup list based on the 1st tup value*)
let compare_first (item1 : (score * movePair)) (item2 : (score * movePair)) =
  compare !(fst item1) !(fst item2)

(*The state (evil, score, board) resulting from shifting
 * [b] in [m] direction with a score of [s] and [e] evil state*)
let move_result m b s evil: staticState =
  let copy = Array.make_matrix 4 4 None in
  let new_score = ref !s in
  to_arr (to_lst b 0) copy 0;
  move m copy new_score;
  {e = evil; score = new_score; board = copy}

(*Sorts a scores_to_moves list [l] from the highest to lowest score*)
let sort_moveList_scores (l : score_to_moves) : score_to_moves =
  List.rev (List.sort compare_first l)

(*Gets the greedy move for the current static state*)
let get_greedy_move (st : staticState) : move =
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

(* Random AI *)
let random_ai b =
  let valid_moves_1 =
      List.filter (fun m -> is_valid_move m b) moveList in
  let rand_ind = Random.int (List.length valid_moves_1) in
  List.nth valid_moves_1 rand_ind


