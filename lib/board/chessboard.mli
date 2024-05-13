(* @author Aidan McNay (acm289) *)

type t
(** The type of a chess board *)

val initial : t
(** [initial] is the initial setup for a chess game. *)

val mk_board : Piece.Pieces.t list -> Move_record.t list -> string list -> t
(** [mk_board pieces records alg_nots] is a board representation of a game with
    [pieces] on board, [records] and [alg_nots] representing the moves already
    made. *)

exception Invalid_move
(** Raised if the user attempts an invalid move. *)

exception Puts_in_check
(** Raised if the user attempts a move that puts them in check. *)

val move_piece :
  t -> Piece.Types.color -> Utils.Location.t -> Utils.Location.t -> t
(** [move_piece board color start finish] is [board] after [color] moves the
    piece at [start] to [finish], capturing a piece on [finish] if necessary.
    Raises: [Invalid_move] if the move isn't a valid one. Raises:
    [Puts_in_check] if the move would put the mover in check. *)

val in_checkmate : Piece.Types.color -> t -> bool
(** [in_checkmate color board] is whether [color] is checkmated on [board]. *)

exception No_moves_made
(** Raised if no moves have been made. *)

val last_move : t -> Move_record.t
(** [last_move board] is the last move made on [board]. Raises: [No_moves_made]
    if no moves have been made. *)

val move_history : t -> Move_record.t list
(** [move_history board] is the complete move history of [board]. The most
    recent move is at the front of the list. *)

val image_at_loc :
  ?selected:bool -> t -> Utils.Location.t -> Bogue.Draw.color -> Bogue.Widget.t
(** [image_at_loc board loc bg] is the image of a piece on [board] at [loc] with
    background [bg]. The image is only the background if no piece is present. *)

val get_rev_alg_notation : t -> string list
(** [get_alg_notation board] is the complete move history of [board] in Standard
    English Algebraic Notation for Chess. The most recent move is at the end of
    the list. *)
