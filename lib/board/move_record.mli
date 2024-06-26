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
  Piece.Types.piece_type option ->
  bool ->
  bool ->
  t
(** [gen_record piece_type color start finish is_check capture castled promoted checkmate en_passant]
    is a record of a [piece_type] piece of color [color] moving from [start] to
    [finish], where [is_check] is whether it placed the opponent in check,
    [captured_a_piece] is whether a piece was captured, [castled] is whether
    this piece castled, [promoted] is whether this move involved a promotion,
    [checkmate] is whether it placed the opponent in checkmate, and [en_passant]
    is whether it was an en passant move. Requires: The provided information
    must represent a valid move, according to the rules for chess. *)

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

val get_en_passant : t -> bool
(** [get_en_passant record] is whether en-passant happened in [record]*)

val get_color : t -> Piece.Types.color
(** [get_color record] is the color of the piece that moved in [record]. *)

val get_checkmate : t -> bool
(** [get_checkmate record] is whether the move in [record] was checkmate. *)
