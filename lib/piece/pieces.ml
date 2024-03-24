(* @author Aidan McNay (acm289) *)

type color = Types.color
type piece_type = Types.piece_type

type t = {
  color : color;
  piece_type : piece_type;
  location : Utils.Location.t;
  metadata : Types.piece_metadata;
}
[@@warning "-69"]
(* Not all used currently *)

let init piece_type color location =
  let metadata =
    match piece_type with
    | Types.Pawn -> Pawn.metadata
    | Types.Knight -> Knight.metadata
    | _ -> failwith "Unimplemented"
  in
  { color; piece_type; location; metadata }

let get_type = failwith "Unimplemented"
let get_color = failwith "Unimplemented"
let get_loc = failwith "Unimplemented"
let get_points = failwith "Unimplemented"
let get_valid_moves = failwith "Unimplemented"
