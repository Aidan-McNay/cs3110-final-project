(* @author Aidan McNay (acm289) *)

type t
(** The type of a record of a move being made. *)

val gen_record :
  Piece.Pieces.t ->
  Utils.Location.t ->
  Utils.Location.t ->
  bool ->
  bool ->
  bool ->
  Piece.Types.piece_type option->
  t
(** [gen_record piece start finish is_check capture castled promoted] is a
    record of [piece] moving from [start] to [finish], where [is_check] is
    whether it placed the opponent in check, [captured_a_piece] is whether a
    piece was captured, [castled] is whether this piece castled, and [promoted]
    is whether this move involved a promotion. *)

val get_piece : t -> Piece.Pieces.t
(** [get_piece record] gets the piece that moved in [record]. *)

val get_start : t -> Utils.Location.t
(** [get_start record] gets the start location of the move in [record]. *)

val get_finish : t -> Utils.Location.t
(** [get_finish record] gets the end location of the move in [record]. *)

val was_check : t -> bool
(** [was_check record] is whether the move in [record] was a check. *)

val was_capture : t -> bool
(** [was_capture record] is whether the move in [record] captured a piece. *)

val was_castle : t -> bool
(** [was_castle record] is whether the move in [record] was a castle. *)

val was_promotion : t -> Piece.Types.piece_type option
(** [was_promotion record] is whether the move in [record] was a promotion. *)
