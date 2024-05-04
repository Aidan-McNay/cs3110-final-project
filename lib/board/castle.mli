(* @author Aidan McNay (acm289) *)

val is_castle : Piece.Types.color -> Piece.Pieces.t -> Utils.Location.t -> bool
(** [is_castle color piece finish] is whether [color] moving [piece] to [finish]
    could only represent a castle. *)

val can_castle :
  Piece.Types.color ->
  Utils.Location.t ->
  Utils.Location.t ->
  Piece.Pieces.t list ->
  Move_record.t list ->
  bool
(** [can_castle color start finish pieces records] is whether [color] can castle
    from [start] to [finish] on a board with [pieces], with the moves in
    [records] already having been made. *)

exception Cant_castle
(** Raised if the player can't castle. *)

val castle :
  Piece.Types.color ->
  Piece.Pieces.t list ->
  Utils.Location.t ->
  Utils.Location.t ->
  Move_record.t list ->
  Piece.Pieces.t list * Move_record.t
(** [castle color pieces start finish records] is the new state after [color]
    castles from [start] to [finish] on a board with [pieces] and past moves
    represented by [records], as well as the accompanying move record. Raises:
    [Cant_castle] if [color] can't perform the castle. *)
