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
(** A type representing the presence of a piece at a location. *)

type valid_moves = piece_presence list -> Utils.Move.moves list
(** The type of a function that gets a piece's valid moves *)

type piece_metadata = {
  points : int;
  get_valid_moves : valid_moves;
}
(** The metadata associated with a chess piece. [points] is how many points the
    piece is worth, and [get_valid_moves] is a function that gets the valid
    moves for a piece. *)
