(* @author Aidan McNay (acm289) *)

val is_castle : Piece.Types.color -> Piece.Pieces.t -> Utils.Location.t -> bool
(** [is_castle color piece finish] is whether [color] moving [piece] to [finish]
    could only represent a castle. *)

val can_castle :
  Piece.Types.color ->
  Piece.Pieces.t ->
  Utils.Location.t ->
  Piece.Pieces.t list ->
  Move_record.t list ->
  bool
(** [can_castle color piece finish pieces records] is whether [color] can castle
    [piece] to [finish] on a board with [pieces], with the moves in [records]
    already having been made. Requires: [is_castle color piece finish] is
    [true]. *)

val castle :
  Piece.Types.color ->
  Piece.Pieces.t list ->
  Piece.Pieces.t ->
  Utils.Location.t ->
  Move_record.t list ->
  Piece.Pieces.t list * Move_record.t
(** [castle color pieces piece finish records] is the new state after [color]
    castles [piece] (a king) to [finish] on a board with [pieces] and past moves
    represented by [records], as well as the accompanying move record. Requires:
    [can_castle color piece finish pieces records] is [true].*)
