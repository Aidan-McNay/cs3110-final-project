(* @author Aidan McNay (acm289) *)

type color = Types.color
type piece_type = Types.piece_type

type t = {
  color : color;
  piece_type : piece_type;
  location : Utils.Location.t;
  metadata : Types.piece_metadata;
}

let init piece_type color location =
  let metadata =
    match piece_type with
    | Types.Pawn -> Pawn.metadata
    | Types.Knight -> Knight.metadata
    | Types.Bishop -> Bishop.metadata
    | Types.Rook -> Rook.metadata
    | Types.Queen -> Queen.metadata
    | Types.King -> King.metadata
  in
  { color; piece_type; location; metadata }

let get_type piece = piece.piece_type
let get_color piece = piece.color
let get_loc piece = piece.location

let set_loc { color; piece_type; location = _; metadata } new_loc =
  { color; piece_type; location = new_loc; metadata }

let get_points piece = piece.metadata.points

(** [piece_to_presence piece] is the presence associated with [piece]. *)
let piece_to_presence (piece : t) : Types.piece_presence =
  { color = piece.color; location = piece.location }

(** [map_to_presences piece_list] is the presences associated with pieces in
    [piece_list]. *)
let map_to_presences piece_list = List.map piece_to_presence piece_list

let get_valid_moves piece piece_list =
  let presence_list = map_to_presences piece_list in
  piece.metadata.get_valid_moves piece.location piece.color presence_list
