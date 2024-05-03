(* @author Aidan McNay (acm289) *)

val get_king : Piece.Types.color -> Piece.Pieces.t list -> Piece.Pieces.t
(** [get_king color pieces] gets the [color] king from [pieces]. *)

val in_check : Piece.Types.color -> Piece.Pieces.t list -> bool
(** [in_check color pieces] is whether [color] is in check on the board with
    [pieces]. *)

val in_checkmate : Piece.Types.color -> Piece.Pieces.t list -> bool
(** [in_checkmate color pieces] is whether [color] is checkmated on the board
    with [pieces]. *)
