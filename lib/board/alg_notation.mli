(* @author Andro Janashia (aj454), Aidan McNay (acm289) *)

val checkmate_entry : Piece.Types.color -> string
(** [checkmate_entry color] is the notation entry for when [color] checkmates
    their opponent. *)

val move_history_to_algformat : string list -> int -> string
(**[move_history_to_string moves acc] transforms the the list [moves] which is a
   list of moves in algebraic notation into a string format with the first
   element being the turn number counting from [acc], second element white's
   turn and third - black's. *)

val move_history_file : string -> string list -> unit
(** [move_history_file filename moves] outputs the PGN format of the notation in
    [moves] to [filename].*)

val move_record_to_alg_notation : Piece.Pieces.t list -> Move_record.t -> string
(** [move_record_to_alg_notation pieces move_record] uses the given
    [move_record] to write a string in the English Standard Algebraic Notation
    format of chess. [pieces] is used to disambiguate which piece made the move
    following the rules of Standard Algebraic Notation *)
