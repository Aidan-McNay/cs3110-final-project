(* @author Aidan McNay (acm289) *)

type color = Types.color
type piece_type = Types.piece_type

type t = {
  color : color;
  piece_type : piece_type;
  metadata : Types.piece_metadata;
}
[@@warning "-69"]
(* Not all used currently *)

let init = failwith "Unimplemented"
let get_type = failwith "Unimplemented"
let get_color = failwith "Unimplemented"
let get_loc = failwith "Unimplemented"
let get_points = failwith "Unimplemented"
let get_valid_moves = failwith "Unimplemented"
