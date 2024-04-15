(* @author Aidan McNay (acm289) *)

val curr_turn : unit -> Piece.Types.color
(** [curr_turn] is the color of who's turn it currently is. *)

val set_indicator : Piece.Types.color -> (unit -> unit) -> unit
(** [set_indicator color f] calls [f ()] whenever it becomes [color]'s turn. *)

exception Not_your_turn of Piece.Types.color
(** Raised if a player tries to make a move when it isn't their turn. *)

val make_move : Piece.Types.color -> unit
(** [make_move color] indicates that the [color] player made a move, switching
    the turn to the other player. Raises [Not_your_turn color] if it isn't
    [color]'s turn. *)
