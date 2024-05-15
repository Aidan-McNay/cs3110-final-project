(* @author Aidan McNay (acm289) *)

type color = Types.color
type piece_type = Types.piece_type

type t = {
  color : color;
  piece_type : piece_type;
  location : Utils.Location.t;
  metadata : Types.piece_metadata;
}
(* AF: The record [{color; piece_types; location; metadata}] represents a piece
   of type [piece_type] and color [color] at the location represented by
   [location] on a chess board. [metadata] contains information about the
   piece's point value, as well as how to determine what moves are valid for
   it. *)
(* RI: [metadata.points] must represent the correct number of points associated
   with the type [piece_type] for chess. Similarly, [metadata.get_valid_moves]
   must correctly determine how a piece of type [piece_type] could move, if
   given the correct color and location. *)

(** [get_metadata piece_type] is the metadata for [piece_type]. *)
let get_metadata = function
  | Types.Pawn -> Pawn.metadata
  | Types.Knight -> Knight.metadata
  | Types.Bishop -> Bishop.metadata
  | Types.Rook -> Rook.metadata
  | Types.Queen -> Queen.metadata
  | Types.King -> King.metadata

let init piece_type color location =
  let metadata = get_metadata piece_type in
  { color; piece_type; location; metadata }

let get_type piece = piece.piece_type
let get_color piece = piece.color
let get_loc piece = piece.location

let set_loc { color; piece_type; location = _; metadata } new_loc =
  { color; piece_type; location = new_loc; metadata }

let set_type { color; piece_type = _; location; metadata = _ } new_type =
  let new_metadata = get_metadata new_type in
  { color; piece_type = new_type; location; metadata = new_metadata }

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

let piece_at_loc piece_list loc =
  let piece_at_loc piece = get_loc piece = loc in
  List.find_opt piece_at_loc piece_list

let start_state =
  [
    (* White *)
    init Rook White (Utils.Location.init_loc 'A' 1);
    init Knight White (Utils.Location.init_loc 'B' 1);
    init Bishop White (Utils.Location.init_loc 'C' 1);
    init Queen White (Utils.Location.init_loc 'D' 1);
    init King White (Utils.Location.init_loc 'E' 1);
    init Bishop White (Utils.Location.init_loc 'F' 1);
    init Knight White (Utils.Location.init_loc 'G' 1);
    init Rook White (Utils.Location.init_loc 'H' 1);
    init Pawn White (Utils.Location.init_loc 'A' 2);
    init Pawn White (Utils.Location.init_loc 'B' 2);
    init Pawn White (Utils.Location.init_loc 'C' 2);
    init Pawn White (Utils.Location.init_loc 'D' 2);
    init Pawn White (Utils.Location.init_loc 'E' 2);
    init Pawn White (Utils.Location.init_loc 'F' 2);
    init Pawn White (Utils.Location.init_loc 'G' 2);
    init Pawn White (Utils.Location.init_loc 'H' 2);
    (* Black *)
    init Rook Black (Utils.Location.init_loc 'A' 8);
    init Knight Black (Utils.Location.init_loc 'B' 8);
    init Bishop Black (Utils.Location.init_loc 'C' 8);
    init Queen Black (Utils.Location.init_loc 'D' 8);
    init King Black (Utils.Location.init_loc 'E' 8);
    init Bishop Black (Utils.Location.init_loc 'F' 8);
    init Knight Black (Utils.Location.init_loc 'G' 8);
    init Rook Black (Utils.Location.init_loc 'H' 8);
    init Pawn Black (Utils.Location.init_loc 'A' 7);
    init Pawn Black (Utils.Location.init_loc 'B' 7);
    init Pawn Black (Utils.Location.init_loc 'C' 7);
    init Pawn Black (Utils.Location.init_loc 'D' 7);
    init Pawn Black (Utils.Location.init_loc 'E' 7);
    init Pawn Black (Utils.Location.init_loc 'F' 7);
    init Pawn Black (Utils.Location.init_loc 'G' 7);
    init Pawn Black (Utils.Location.init_loc 'H' 7);
  ]

let to_image piece bg =
  let color =
    match piece.color with
    | Types.Black -> "black"
    | Types.White -> "white"
  in
  let name =
    match piece.piece_type with
    | Types.Pawn -> "pawn"
    | Types.Knight -> "knight"
    | Types.Bishop -> "bishop"
    | Types.Rook -> "rook"
    | Types.Queen -> "queen"
    | Types.King -> "king"
  in
  let filename = "icons/" ^ name ^ "_" ^ color ^ ".png" in
  ignore bg;
  Bogue.Widget.image ~w:50 ~h:50 ~bg filename
