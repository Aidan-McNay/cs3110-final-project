(* @author Aidan McNay (acm289) *)

val in_check : Piece.Types.color -> Piece.Pieces.t list -> bool
(** [in_check color pieces] is whether [color] is in check on the board with
    [pieces]. *)
