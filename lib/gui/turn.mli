(* @author Aidan McNay (acm289) *)

val curr_turn : unit -> Piece.Types.color
(** [curr_turn] is the color of who's turn it currently is. *)

val set_callback : Piece.Types.color option -> (unit -> unit) -> unit
(** [set_callback color f] calls [f ()] whenever it becomes [color]'s turn. If
    [color] is [None], then the callback will trigger whenever the turn changes. *)

val call_callbacks : Piece.Types.color option -> unit
(** [call_callbacks color] calls the callbacks for [color]. If [color] is
    [None], calls the callbacks for both colors. *)

exception Not_your_turn of Piece.Types.color
(** Raised if a player tries to make a move when it isn't their turn. *)

val make_move : Piece.Types.color -> unit
(** [make_move color] indicates that the [color] player made a move, switching
    the turn to the other player. Raises [Not_your_turn color] if it isn't
    [color]'s turn. *)
