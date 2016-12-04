open OUnit2
open Gameplay
(* open Main *)

let gameplay_tests = [
  (* TEST GAMEPLAY *)
  "Check square_value 0" >:: (fun _ -> assert_equal
    0
    (square_value (None)));
  "Check square_value 2" >:: (fun _ -> assert_equal
    2
    (square_value (Some 2)));
  "Check is_empty_square True" >:: (fun _ -> assert_equal
    true
    (is_empty_square (None)));
  "Check is_empty_square False" >:: (fun _ -> assert_equal
    false
    (is_empty_square (Some 8)));
  "Check init" >:: (fun _ -> assert_equal
      {
        evil = ref false;
        s = ref 0;
        b = [|[|None; None; None; None|];
        [|None; None; None; None|];
        [|None; None; None; None|];
        [|None; None; None; Some 2|]|]
      }
    (init_board  4));
  "Check if 2048 square is 2048 square" >:: (fun _ -> assert_equal
    true
    (check_2048_square (Some 2048)));
  "Check if non 2048 square is 2048 square" >:: (fun _ -> assert_equal
    false
    (check_2048_square (Some 4)));
  "Check winning board" >:: (fun _ -> assert_equal
    true
    (check_winning_board [|[|None; None; None; None|];
    [|None; None; None; None|];
    [|None; None; None; None|];
    [|None; None; None; Some 2048|]|]));
  "Check non winning board" >:: (fun _ -> assert_equal
    false
    (check_winning_board [|[|None; None; None; None|];
    [|None; None; None; None|];
    [|None; None; None; Some 1048|];
    [|None; None; None; Some 1048|]|]));
  "Check empty squares" >:: (fun _ -> assert_equal
    [(0, 1); (0, 2); (1, 1); (1, 2); (1, 3); (2, 0); (2, 3); (3, 0); (3, 1)]
    (empty_squares [|[|Some 4; None; None; Some 4|];
    [|Some 2; None; None; None|];
    [|None; Some 2; Some 4; None|];
    [|None; None; Some 2; Some 2|]|]));
  "Check empty squares" >:: (fun _ -> assert_equal
    []
    (empty_squares [|[|Some 4; Some 4; Some 4; Some 4|];
    [|Some 2; Some 4; Some 4; Some 4|];
    [|Some 4; Some 2; Some 4; Some 4|];
    [|Some 4; Some 4; Some 2; Some 2|]|]));
  "Find max square" >:: (fun _ -> assert_equal
    (3,2)
    (find_max_sq [|[|Some 4; Some 4; Some 4; Some 4|];
    [|Some 2; Some 4; Some 4; Some 4|];
    [|Some 4; Some 2; Some 4; Some 4|];
    [|Some 4; Some 4; Some 8; Some 2|]|]));
  "Find max square" >:: (fun _ -> assert_equal
    (0,0)
    (find_max_sq [|[|Some 4; Some 4; Some 4; Some 4|];
    [|Some 2; Some 4; Some 4; Some 4|];
    [|Some 4; Some 2; Some 4; Some 4|];
    [|Some 4; Some 4; Some 2; Some 2|]|]));

(*   [|[|Some 4; None; None; Some 4|];
    [|Some 2; None; None; None|];
    [|None; Some 2; Some 4; None|];
    [|None; None; Some 2; Some 2|]|]

    [|[|Some 8; None; None; None|];
      [|Some 2; None; None; None|];
      [|Some 2; Some 4; None; None|];
      [|Some 4; None; None; None|]|]  *)

]

let my_tests = [
  (* put any tests you like here; they will not be graded *)
]

let _ = run_test_tt_main ("suite" >::: gameplay_tests @ my_tests)