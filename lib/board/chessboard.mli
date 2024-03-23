(* @author Aidan McNay (acm289) *)

type t
(** The type of a chess board *)

val initial : t
(** [initial] is the initial setup for a chess game. *)

val get_points : t -> Piece.Pieces.color -> int
(** [get_points board color] gets the points of pieces that the [color] player
    has captured on [board]. *)

exception Invalid_move
(** Raised if the user attempts an invalid move. *)

type move_record = {
  piece : Piece.Pieces.t;
  start : Utils.Location.t;
  finish : Utils.Location.t;
  is_check : bool;
  captured_a_piece : bool;
}

val move_piece : t -> Utils.Location.t -> Utils.Location.t -> t * move_record
(** [move_piece board start finish] is [board] after moving the piece at [start]
    to [finish], capturing a piece on [finish] if necessary, as well as a record
    of the move. Raises: [Invalid_move] if the move isn't a valid one. *)

val in_check : t -> Piece.Pieces.color -> bool
(** [in_check board color] is whether the player with [color] pieces is in check
    on the current board. *)

exception No_moves_made
(** Raised if no moves have been made. *)

val last_move : t -> move_record
(** [last_move board] is the last move made on [board]. Raises: [No_moves_made]
    if no moves have been made. *)

val move_history : t -> move_record list
(** [move_history board] is the complete move history of [board]. The most
    recent move is at the front of the least. Raises: [No_moves_made] if no
    moves have been made. *)
