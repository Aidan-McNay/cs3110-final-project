(* @author Aidan McNay (acm289), Henry Toll (hht26) *)

val start : unit -> unit
(** [start] is the infinite sequence of the chess game*)

exception Has_won of Piece.Pieces.color
(** [Has_Won] is an exception that specifies that this color has won*)

val make_move :
  Board.Chessboard.t ->
  Piece.Types.color ->
  Utils.Location.t ->
  Utils.Location.t ->
  Board.Chessboard.t
(** [make_move] will make a move if the move is a legal move according to chess
    rules. Raises [Has_won color] if the move makes [color] win. *)
