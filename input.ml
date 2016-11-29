open Gameplay

type action = Move of move | Invalid

let key_to_action = function
  | 'i' -> Move Up
  | 'k' -> Move Down
  | 'j' -> Move Left
  | 'l' -> Move Right
  | _ -> Invalid

let action () = Graphics.read_key () |> key_to_action