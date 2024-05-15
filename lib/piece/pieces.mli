(* @author Aidan McNay (acm289) *)

type t
(** The type of a chess piece *)

val init : Types.piece_type -> Types.color -> Utils.Location.t -> t
(** [init piece_type piece_color piece_loc] is a piece of type [piece_type] and
    color [piece_color], currently at [piece_loc]. *)

val get_type : t -> Types.piece_type
(** [get_type piece] is the type of [piece]. *)

val get_color : t -> Types.color
(** [get_color piece] is the color of [piece]. *)

val get_loc : t -> Utils.Location.t
(** [get_loc piece] is the location of [piece]. *)

val set_loc : t -> Utils.Location.t -> t
(** [set_loc piece loc] is [piece] at [loc]. *)

val set_type : t -> Types.piece_type -> t
(** [set_type piece piece_type] is [piece] with type [piece_type]. *)

val get_points : t -> int
(** [get_points piece] is the number of points that [piece] is worth. *)

val get_valid_moves : t -> t list -> Utils.Move.moves list
(** [get_valid_moves piece state] is the valid moves that [piece] can take,
    given a board state of [state]. *)

val piece_at_loc : t list -> Utils.Location.t -> t option
(** [piece_at_loc piece_list loc] is the piece in [piece_list] at [loc], if any. *)

val start_state : t list
(** [start_state] is a list of the pieces on a board at the start of a game. *)

val to_image : t -> Bogue.Draw.color -> Bogue.Widget.t
(** [to_image piece bg] is the image representation of [piece] with background
    [bg]. *)
