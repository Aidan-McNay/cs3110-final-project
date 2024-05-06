(* @author Andro Janashia (aj454) *)

val move_record_to_longalgnotation : Move_record.t -> string
(**[move_record_to_longalgnotation move_rec] converts move record [move_rec] 
into a long algebraic notation *)
  
val move_history_to_algformat : Move_record.t list -> int -> string
(**[move_history_to_string moves acc] transforms the the list [moves] which is a 
list of move recors into a string format with the first element 
being the turn number counting from [acc], second element white's turn 
and third - black's. *)

val print_move_history : out_channel -> Move_record.t list -> unit
(**[print_move_history out_channel moves] prints out the [moves] made by the players
    during the game to [out_channel], with "Turns, White, Black" as the header
    and the moves in English Standard Algebraic Notation*)

val move_history_file : string  -> Move_record.t list -> unit
(**[move_history_file filename moves] returns a file [filename] with the [moves] 
    printed by the [print_move_history] function.*)

val move_record_to_alg_notation : 
Piece.Pieces.t list -> 
Piece.Types.piece_type -> 
Utils.Location.t -> 
Utils.Location.t -> 
bool ->
bool ->
Piece.Types.piece_type option ->
bool ->    
string
(** [move_record_to_alg_notation pieces piece_type start finish check capture promotion checkmate] 
Uses the given arguments to write a string in the English Standard Algebraic Notation format of chess. 
[pieces] is used to disambiguate which piece made the move following the rules of Standard Algebraic
Notation *)

val en_passant_to_notation : Utils.Location.t -> Utils.Location.t -> string
(** [en_passant_to_notation start finish] returns English standard Algebraic Notation
    form of En Passant, wher the pawn moved from [start] to [finish]*)

val castling_to_notation : Utils.Location.t -> Utils.Location.t -> Piece.Types.color -> string
(** [castling_to_notation start finish color] checks whether kingside castling
    happened or queenside and returns the English Standard Algebraic Notation
    associated with what happened.*)