(* @author Aidan McNay (acm289) *)

(** The colors that a chess piece can be *)
type color =
  | White
  | Black

val opposite : color -> color
(** [opposite color] is the opposite color of [color]. *)

val str_of_color : color -> string
(** [str_of_color color] is the string representation of [color]. *)

(** The different kinds of piece that a chess piece can be *)
type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

val str_of_type : piece_type -> string
(** [str_of_type piece_type] is the string representation of [piece_type]. *)

type piece_presence = {
  color : color;
  location : Utils.Location.t;
}
(** A type representing the presence of a piece at a location. *)

val color_present : piece_presence list -> Utils.Location.t -> color option
(** [color_present presence_list location] is the color of the piece in
    [presence_list] at [location], if any. *)

val get_dir_moves :
  Utils.Location.t ->
  color ->
  piece_presence list ->
  Utils.Move.moves ->
  Utils.Move.moves list ->
  Utils.Move.moves list
(** [get_dir_moves loc color presence_list dir moves_list] adds to [move_list]
    all moves that a piece of color [color] at [loc] can take in [dir]
    direction, given the pieces in [presence_list] and assuming that the piece
    can take pieces of the other color. *)

type valid_moves =
  Utils.Location.t -> color -> piece_presence list -> Utils.Move.moves list
(** The type of a function that gets a piece's valid moves.
    [valid_moves loc presence_list] is all of the valid moves that a piece at
    [loc] can take, given pieces at [presence_list]. *)

type piece_metadata = {
  points : int;
  get_valid_moves : valid_moves;
}
(** The metadata associated with a chess piece. [points] is how many points the
    piece is worth, and [get_valid_moves] is a function that gets the valid
    moves for a piece. *)
