(* @author Andro Janashia (aj454) *)

val move_record_to_longalgnotation : Move_record.t -> string
(**[move_record_to_longalgnotation move_rec] converts move record [move_rec]
   into a long algebraic notation *)

val move_history_to_algformat : string list -> int -> string
(**[move_history_to_string moves acc] transforms the the list [moves] which is a
   list of moves in algebraic notation into a string format with the first
   element being the turn number counting from [acc], second element white's
   turn and third - black's. *)

val print_move_history : out_channel -> string list -> unit
(**[print_move_history out_channel moves] prints out the [moves] made by the
   players during the game to [out_channel], with "Turns, White, Black" as the
   header and the moves in English Standard Algebraic Notation*)

val move_history_file : string -> string list -> unit
(**[move_history_file filename moves] returns a file [filename] with the [moves]
   printed by the [print_move_history] function.*)

val move_record_to_alg_notation : Piece.Pieces.t list -> Move_record.t -> string
(** [move_record_to_alg_notation pieces move_record] uses the given
    [move_record] to write a string in the English Standard Algebraic Notation
    format of chess. [pieces] is used to disambiguate which piece made the move
    following the rules of Standard Algebraic Notation *)
