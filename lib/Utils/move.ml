(* @author Aidan McNay (acm289) *)

type move =
  | Up
  | Down
  | Left
  | Right

type moves = move list

let to_string move =
  match move with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
