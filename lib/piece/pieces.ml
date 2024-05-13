(* @author Aidan McNay (acm289) *)

type color = Types.color
type piece_type = Types.piece_type

type t = {
  color : color;
  piece_type : piece_type;
  location : Utils.Location.t;
  metadata : Types.piece_metadata;
}

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

(** [to_string_black piece] is the string representation of [piece], assuming
    that the piece is Black. *)
let to_string_black piece =
  match piece.piece_type with
  | Types.Pawn -> "\u{265F}"
  | Types.Knight -> "\u{265E}"
  | Types.Bishop -> "\u{265D}"
  | Types.Rook -> "\u{265C}"
  | Types.Queen -> "\u{265B}"
  | Types.King -> "\u{265A}"

(** [to_string_white piece] is the string representation of [piece], assuming
    that the piece is White. *)
let to_string_white piece =
  match piece.piece_type with
  | Types.Pawn -> "\u{2659}"
  | Types.Knight -> "\u{2658}"
  | Types.Bishop -> "\u{2657}"
  | Types.Rook -> "\u{2656}"
  | Types.Queen -> "\u{2655}"
  | Types.King -> "\u{2654}"

let to_string piece =
  match piece.color with
  | Types.Black -> to_string_black piece
  | Types.White -> to_string_white piece

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
