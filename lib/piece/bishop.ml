(* @author Andro Janashia (aj454) *)

let bishop_valid_moves loc presence_list = failwith "Unimplemented"

let metadata : Types.piece_metadata =
  { points = 3; get_valid_moves = bishop_valid_moves }
