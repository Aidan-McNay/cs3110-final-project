(* @author Andro Janashia (aj454) *)

val move_record_to_longalgnotation : Move_record.t -> string
(**[move_record_to_longalgnotation move_rec] converts move record [move_rec] 
into a long algebraic notation *)

val alg_notation_move_history : Move_record.t list -> string list
(**[algnotation_move_history moves] takes the move record list [moves] from board 
and turns it into a list of long algebraic notation *)  
  
val move_history_to_string : string list -> int -> string
(**[move_history_to_string moves acc] transforms the the list [moves] which is a 
list of moves in algebraic notations into a string format with the first 
element being the turn number counting from [acc], second element white's turn 
and third - black's. *)

val print_move_history : out_channel -> string list -> unit
(**[print_move_history out_channel moves] prints out the [moves] made by the players
    during the game to [out_channel], with "Turns, White, Black" as the header*)

val move_history_file : string  -> string list -> unit
(**[move_history_file filename moves] returns a file [filename] with the [moves] 
    printed by the [print_move_history] function.*)

val move_record_to_alg_notation : Chessboard.t -> Move_record.t -> string
(** [move_record_to_alg_notation board move_rec] converts a move record [move_rec] in [board] to
a string of the English Standard Algebraic Notation format of chess. *)