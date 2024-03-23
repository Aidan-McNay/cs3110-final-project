(* @author Aidan McNay (acm289) *)

(** The colors that a chess piece can be *)
type color =
  | White
  | Black

(** The different kinds of piece that a chess piece can be *)
type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type piece_presence = {
  color : color;
  location : Utils.Location.t;
}

type valid_moves = piece_presence list -> Utils.Move.moves list

type piece_metadata = {
  points : int;
  get_valid_moves : valid_moves;
}
(** The type of a chess piece *)
