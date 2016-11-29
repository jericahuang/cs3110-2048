open OUnit2
open Gameplay

let public_tests = [
  (* TEST GAMEPLAY *)
  (* "Check winning board" >:: (fun _ -> assert_equal
    true
    (interp 1 "FD 10"));
  "Check non winning board" >:: (fun _ -> assert_equal
    false
    (interp 1 "FD 10")); *)
  "Check if 2048 square is 2048 square" >:: (fun _ -> assert_equal
    true
    (check_2048_square (Some 2048)));
  "Check if non 2048 square is 2048 square" >:: (fun _ -> assert_equal
    false
    (check_2048_square (Some 4)));
  "Check init" >:: (fun _ -> assert_equal
    [|[|None; None; None; None|];
    [|None; None; None; None|];
    [|None; None; None; None|];
    [|None; None; None; Some 2|]|]
    (init_board  4));

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

let _ = run_test_tt_main ("suite" >::: public_tests @ my_tests)