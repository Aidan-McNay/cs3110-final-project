(* @author Aidan McNay (acm289) *)

val widget_of_image :
  Board.Chessboard.t -> Utils.Location.t -> Bogue.Draw.color -> Bogue.Widget.t
(** [widget_of_image board loc bg] is the widget representing the piece on
    [board] at [loc] with background [bg]. *)
