(* @author Aidan McNay (acm289) *)

type t
(** The type of a location on a chess board. *)

exception Off_board
(** Raised if a location isn't a valid location on a chessboard. *)

val init_loc : char -> int -> t
(** [init_loc col row] is the location with column [col] and row [row]. Raises:
    [Off_board] if [col] isn't a letter between A and H, inclusive and
    case-insensitive. Raises: [Off_board] if [row] isn't a number between 1 and
    8, inclusive. *)

val get_col : t -> char
(** [get_col loc] is the column value of [loc]. *)

val get_row : t -> int
(** [get_rw loc] is the row value of [loc]. *)

val apply_move : t -> Move.move -> t
(** [apply_move loc move] is the location moved by [move]. Raises: [Off_board]
    if the result of the move goes off of the board (defined by columns A-H and
    rows 1-8). *)

val apply_moves : t -> Move.moves -> t
(** [apply_moves loc moves] is the location moved by [moves], in order. Raises:
    [Off_board] if any of the intermediate locations are off the board (defined
    by columns A-H and rows 1-8). *)

val str_of_loc : t -> string
(** [str_of_loc loc] is the string representation of [loc]. *)
