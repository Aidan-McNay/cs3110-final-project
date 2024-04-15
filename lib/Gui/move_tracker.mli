(* @author Aidan McNay (acm289) *)

type t
(** The type of a move tracker, which translates clicks on the GUI to moves on
    the board. *)

val init : Board.Chessboard.t ref -> Piece.Types.color -> t
(** [init board_ref color] is a new move tracker, tracking moves made by [color]
    for the board pointed to by [board_ref]. *)

val log_click : t -> Utils.Location.t -> unit
(** [log_click tracker loc] logs a click at [loc] with [tracker]. Has no effect
    if it isn't the color's turn. *)
