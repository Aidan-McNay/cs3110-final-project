(* @author Aidan McNay (acm289) *)

type color = Types.color
(** The colors that a chess piece can be *)

type piece_type = Types.piece_type
(** The different kinds of piece that a chess piece can be *)

type t
(** The type of a chess piece *)

val init : piece_type -> color -> Utils.Location.t -> t
(** [init piece_type piece_color piece_loc] is a piece of type [piece_type] and
    color [piece_color], currently at [piece_loc]. *)

val get_type : t -> piece_type
(** [get_type piece] is the type of [piece]. *)

val get_color : t -> color
(** [get_color piece] is the color of [piece]. *)

val get_loc : t -> Utils.Location.t
(** [get_loc piece] is the location of [piece]. *)

val set_loc : t -> Utils.Location.t -> t
(** [set_loc piece loc] is [piece] at [loc]. *)

val set_type : t -> piece_type -> t
(** [set_type piece piece_type] is [piece] with type [piece_type]. *)

val get_points : t -> int
(** [get_points piece] is the number of points that [piece] is worth. *)

val get_valid_moves : t -> t list -> Utils.Move.moves list
(** [get_valid_moves piece state] is the valid moves that [piece] can take,
    given a board state of [state]. *)

val piece_at_loc : t list -> Utils.Location.t -> t option
(** [piece_at_loc piece_list loc] is the piece in [piece_list] at [loc], if any. *)

val to_string : t -> string
(** [to_string piece] is the string representation of [piece]. *)

val to_image : t -> Bogue.Draw.color -> Bogue.Widget.t
(** [to_image piece bg] is the image representation of [piece] with background
    [bg]. *)

val to_algebraic_notation : t -> string
(** [to_algebraic_notation] is the string representation of [piece] using the
    English Standard Algebraic Notation of Chess*)

val alg_notation_to_piece_type : char -> piece_type
(** [to_alg_notation_to_piece_type] takes the algebraic notation character
    symbol of a piece and converts it into [piece_type]*)
