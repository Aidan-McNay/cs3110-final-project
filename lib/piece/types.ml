(* @author Aidan McNay (acm289) *)

(** The colors that a chess piece can be *)
type color =
  | White
  | Black

let str_of_color = function
  | White -> "White"
  | Black -> "Black"

let opposite color =
  match color with
  | Black -> White
  | White -> Black

(** The different kinds of piece that a chess piece can be *)
type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

let str_of_type = function
  | Pawn -> "Pawn"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Rook -> "Rook"
  | Queen -> "Queen"
  | King -> "King"

type piece_presence = {
  color : color;
  location : Utils.Location.t;
}

let color_present presence_list location =
  match
    List.find_opt (fun presence -> presence.location = location) presence_list
  with
  | Some p -> Some p.color
  | None -> None

(** [get_dir_moves_aux curr_moves loc color presence_list dir moves_list] adds
    to [move_list] all moves that a piece of color [color] at [loc] can take in
    [dir] direction, given the pieces in [presence_list] and assuming that the
    piece can take pieces of the other color, as well as that the piece has
    already moved [curr_moves] to get to [loc]. *)
let rec get_dir_moves_aux curr_moves loc color presence_list dir moves_list =
  try
    let new_loc = Utils.Location.apply_moves loc dir in
    match color_present presence_list new_loc with
    | Some target_color ->
        if color <> target_color then (curr_moves @ dir) :: moves_list
        else moves_list
    | None ->
        let moves_to_loc = curr_moves @ dir in
        get_dir_moves_aux moves_to_loc new_loc color presence_list dir
          (moves_to_loc :: moves_list)
  with Utils.Location.Off_board -> moves_list

let get_dir_moves = get_dir_moves_aux []

type valid_moves =
  Utils.Location.t -> color -> piece_presence list -> Utils.Move.moves list

type piece_metadata = {
  points : int;
  get_valid_moves : valid_moves;
}
(** The type of a chess piece *)
