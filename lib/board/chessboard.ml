(* @author Aidan McNay (acm289) *)

type move_record = {
  piece : Piece.Pieces.t;
  start : Utils.Location.t;
  finish : Utils.Location.t;
  is_check : bool;
  captured_a_piece : bool;
}

type t = {
  pieces_on_board : Piece.Pieces.t list;
  captured_by_white : Piece.Pieces.t list;
  captured_by_black : Piece.Pieces.t list;
  moves : move_record list;
}
[@@warning "-69"]
(* Usused currently *)

(* AF: The record [{pieces_on_board = p; captured_by_white = w;
   captured_by_black = b; moves = m}] represents a chess board with the pieces
   in [p] on the board, with white having caputed the pieces in [w] and black
   having captured the pieces in [b]. [m] is a list of [move_record]s that have
   occured so far in the game. *)
(* RI: The location must be a valid chess board location; [row] and [column]
   must be between 1 and 8, inclusive. *)

let start_pieces_list =
  Piece.Pieces.
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

let initial =
  {
    pieces_on_board = start_pieces_list;
    captured_by_white = [];
    captured_by_black = [];
    moves = [];
  }

(** [get_piece_points pieces] is the cumulative number of points that the pieces
    in [pieces] are worth. *)
let get_piece_points pieces =
  List.fold_left (fun acc piece -> acc + Piece.Pieces.get_points piece) 0 pieces

let get_points board color =
  match color with
  | Piece.Types.White -> get_piece_points board.captured_by_white
  | Piece.Types.Black -> get_piece_points board.captured_by_black

exception Invalid_move

let move_piece = failwith "Unimplemented"
let in_check = failwith "Unimplemented"

exception No_moves_made

let last_move = failwith "Unimplemented"
let move_history = failwith "Unimplemented"
