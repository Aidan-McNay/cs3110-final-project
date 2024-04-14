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
  Bogue.Image.create ~bg filename
