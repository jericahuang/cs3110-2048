open OUnit2
open Gameplay

let public_tests = [
  (* TEST GAMEPLAY *)
  "Check winning board" >:: (fun _ -> assert_equal
    true
    (interp 1 "FD 10"));
  "Check non winning board" >:: (fun _ -> assert_equal
    false
    (interp 1 "FD 10"));
]