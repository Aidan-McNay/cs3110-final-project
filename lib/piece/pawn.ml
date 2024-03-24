(* @author Aidan McNay (acm289) *)

let pawn_valid_moves loc presence_list = failwith "Unimplemented"

let metadata : Types.piece_metadata =
  { points = 1; get_valid_moves = pawn_valid_moves }
