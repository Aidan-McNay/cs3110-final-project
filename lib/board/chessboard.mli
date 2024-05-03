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

val move_piece :
  t -> Piece.Types.color -> Utils.Location.t -> Utils.Location.t -> t
(** [move_piece board color start finish] is [board] after [color] moves the
    piece at [start] to [finish], capturing a piece on [finish] if necessary.
    Raises: [Invalid_move] if the move isn't a valid one. *)

exception No_moves_made
(** Raised if no moves have been made. *)

val last_move : t -> Move_record.t
(** [last_move board] is the last move made on [board]. Raises: [No_moves_made]
    if no moves have been made. *)

val move_history : t -> Move_record.t list
(** [move_history board] is the complete move history of [board]. The most
    recent move is at the front of the list. *)

val string_rep : t -> string
(** [string_rep board] is the string representation of [board]. *)

val image_at_loc :
  ?selected:bool -> t -> Utils.Location.t -> Bogue.Draw.color -> Bogue.Widget.t
(** [image_at_loc board loc bg] is the image of a piece on [board] at [loc] with
    background [bg]. The image is only the background if no piece is present. *)
