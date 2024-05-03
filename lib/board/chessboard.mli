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
  castled : bool;
  promoted : bool;
}

val get_start_move_record : move_record -> Utils.Location.t
val get_in_check_move_record : move_record -> bool

val move_piece : t -> Utils.Location.t -> Utils.Location.t -> t * move_record
(** [move_piece board start finish] is [board] after moving the piece at [start]
    to [finish], capturing a piece on [finish] if necessary, as well as a record
    of the move. Raises: [Invalid_move] if the move isn't a valid one. *)

val move_piece_castle : t -> Utils.Location.t -> Utils.Location.t -> t * move_record

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
    recent move is at the front of the list. *)

val string_rep : t -> string
(** [string_rep board] is the string representation of [board]. *)

val piece_at_loc : Piece.Pieces.t list -> Utils.Location.t -> Piece.Pieces.t option
(** [piece_at_loc] is the piece at a location given a list of pieces*)

val get_pieces_on_board : t -> Piece.Pieces.t list
(** [get_pieces_onboard] is the list of all pieces on a board given a board t*)

val checkmate : t -> Piece.Types.color -> bool
(**[checkmate] is a boolean that represents whether Piece.Types.color has lost*)

val check_board_for_promotion: t -> Piece.Types.color -> Utils.Location.t * bool

val promote : t -> Utils.Location.t -> Piece.Types.piece_type -> Piece.Types.color -> t

val image_at_loc :
  ?selected:bool -> t -> Utils.Location.t -> Bogue.Draw.color -> Bogue.Widget.t
(** [image_at_loc board loc bg] is the image of a piece on [board] at [loc] with
    background [bg]. The image is only the background if no piece is present. *)
