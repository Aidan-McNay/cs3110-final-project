(* @author Aidan McNay (acm289) *)

type t
(** The type of a record of a move being made. *)

val gen_record :
  Piece.Types.piece_type ->
  Piece.Types.color ->
  Utils.Location.t ->
  Utils.Location.t ->
  bool ->
  bool ->
  bool ->
  Piece.Types.piece_type option->
  bool ->
  string ->
  t
(** [gen_record piece start finish is_check capture castled promoted checkmate alg_notation] is a
    record of [piece] moving from [start] to [finish], where [is_check] is
    whether it placed the opponent in check, [captured_a_piece] is whether a
    piece was captured, [castled] is whether this piece castled, [promoted]
    is whether this move involved a promotion, [checkmate] is whether it placed the 
    opponent in checkmate and [alg_notation] is the string representation of the 
    move in English Standard Algebraic Notation. *)

val get_piece_type : t -> Piece.Types.piece_type
(** [get_piece record] gets the type of the piece that moved in [record]. *)

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

val get_alg_not : t -> string
(** [get_alg_not record] is the representation of the [record] in English
    Standard Algebraic Information*)

val get_color : t -> Piece.Types.color
(** [get_color record] is the color of the piece that moved in [record]*)

val get_checkmate : t -> bool
(** [get_checkmate record] is whether the move in [record] was checkmate*)