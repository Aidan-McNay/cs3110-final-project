(* @author Aidan McNay (acm289) *)

(* The type of a move on a chessboard *)
type move =
  | Up
  | Down
  | Left
  | Right

(* Multiple moves are expressed as a list *)
type moves = move list

val to_string : move -> string
(** [to_string move] is the string representation of [move]. *)
