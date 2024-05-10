(* @author Aidan McNay (acm289) *)

val is_en_passant :
  Piece.Types.color ->
  Piece.Pieces.t ->
  Utils.Location.t ->
  Piece.Pieces.t list ->
  bool
(** [is_en_passant color piece finish pieces] is whether [color] moving [piece]
    to [finish] on a board with [pieces] could only represent an en passant.
    Requires: [Piece.Pieces.get_color piece = color]*)

val can_en_passant :
  Piece.Types.color ->
  Piece.Pieces.t ->
  Utils.Location.t ->
  Move_record.t list ->
  bool
(** [can_castle color piece finish pieces records] is whether [color] can en
    passant [piece] to [finish], with the moves in [records] already having been
    made. Requires: [is_en_passant color piece finish pieces] is [true]. *)

exception Cant_en_passant
(** Raised if the player can't en passant. *)

val en_passant :
  Piece.Types.color ->
  Piece.Pieces.t list ->
  Piece.Pieces.t ->
  Utils.Location.t ->
  Move_record.t list ->
  Piece.Pieces.t list * Move_record.t * Piece.Pieces.t
(** [castle color pieces piece finish records] is the new state after [color]
    castles from [start] to [finish] on a board with [pieces] and past moves
    represented by [records], as well as the accompanying move record and the
    captured pawn. Requires: [can_en_passant color piece finish pieces] is
    [true]. *)
