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
    (check_2048_square {value = (Some 2048)}));
  "Check if non 2048 square is 2048 square" >:: (fun _ -> assert_equal
    false
    (check_2048_square {value = (Some 4)}));
]