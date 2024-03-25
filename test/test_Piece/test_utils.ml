(* @author Aidan McNay (acm289) *)

open OUnit2

(* [empty_board] is the empty list of pieces on a board. *)
let empty_board : Piece.Pieces.t list = []

(* [board1] is the list of pieces corresponding to
   https://lichess.org/editor/2b5/4Bpbp/7r/p1Np4/2p2P1P/5P1p/1k1P4/1B3R1K_w_-_-_0_1?color=white *)
let board1 =
  Piece.Pieces.
    [
      init Bishop White (Utils.Location.init_loc 'B' 1);
      init Rook White (Utils.Location.init_loc 'F' 1);
      init King White (Utils.Location.init_loc 'H' 1);
      init King Black (Utils.Location.init_loc 'B' 2);
      init Pawn White (Utils.Location.init_loc 'D' 2);
      init Pawn White (Utils.Location.init_loc 'F' 3);
      init Pawn Black (Utils.Location.init_loc 'H' 3);
      init Pawn Black (Utils.Location.init_loc 'C' 4);
      init Pawn White (Utils.Location.init_loc 'F' 4);
      init Pawn White (Utils.Location.init_loc 'H' 4);
      init Pawn Black (Utils.Location.init_loc 'A' 5);
      init Knight White (Utils.Location.init_loc 'C' 5);
      init Pawn Black (Utils.Location.init_loc 'D' 5);
      init Rook Black (Utils.Location.init_loc 'H' 6);
      init Bishop White (Utils.Location.init_loc 'E' 7);
      init Pawn Black (Utils.Location.init_loc 'F' 7);
      init Bishop Black (Utils.Location.init_loc 'G' 7);
      init Pawn Black (Utils.Location.init_loc 'H' 7);
      init Bishop Black (Utils.Location.init_loc 'C' 8);
    ]

(** The type of module for providing test inputs for a given piece. *)
module type PieceTestInputs = sig
  val piece_type : Piece.Types.piece_type
  (** [piece_type] is the type of the piece we're testing*)

  val points : int
  (** [points] is the expected number of points the piece should have. *)

  val empty_location : Utils.Location.t
  (** [empty_location] is the location we're testing on [empty]. *)

  val empty_color : Piece.Types.color
  (** [empty_color] is the color of the piece we're testing on [empty]. *)

  val empty_possible_moves : Utils.Move.moves list
  (** [empty_possible_moves] is the possible moves the piece should have from
      [empty_location] on [empty]. *)

  val board1_location : Utils.Location.t
  (** [board1_location] is the location we're testing on [board1]. *)

  val board1_color : Piece.Types.color
  (** [board1_color] is the color of the piece we're testing on [board1]. *)

  val board1_possible_moves : Utils.Move.moves list
  (** [board1_possible_moves] is the possible moves the piece should have from
      [board1_location] on [board1]. *)
end

(** [assert_contents_equal lst1 lst2] is an assertion function for OUnit to
    check whether [lst1] and [lst2] have the same contents, even if in a
    different order. *)
let assert_move_contents_equal lst1 lst2 _ =
  let lst1_elements_in_lst2 lst1 lst2 =
    List.iter
      (fun el -> assert_equal (List.exists (fun x -> x = el) lst2) true)
      lst1
  in
  lst1_elements_in_lst2 lst1 lst2;
  lst1_elements_in_lst2 lst2 lst1

(** [moves_list_printer moves_list] is the string representation of
    [moves_list]. *)
let moves_list_printer moves_list =
  let string_moves_list = List.map (List.map Utils.Move.to_string) moves_list in
  let moves_string_list = List.map (String.concat "-") string_moves_list in
  String.concat " " moves_string_list

(** A functor for testing a piece, based on test inputs of type
    [PieceTestInputs]. *)
module PieceTester (Inputs : PieceTestInputs) = struct
  let empty_board_piece =
    Inputs.(Piece.Pieces.init piece_type empty_color empty_location)

  let board1_piece =
    Inputs.(Piece.Pieces.init piece_type board1_color board1_location)

  let basic_check piece color location _ =
    assert_equal Inputs.piece_type (Piece.Pieces.get_type piece);
    assert_equal color (Piece.Pieces.get_color piece);
    assert_equal location (Piece.Pieces.get_loc piece);
    assert_equal Inputs.points (Piece.Pieces.get_points piece);
    let loc1 = Utils.Location.init_loc 'B' 8 in
    let loc2 = Utils.Location.init_loc 'G' 3 in
    assert_equal loc1 (Piece.Pieces.get_loc (Piece.Pieces.set_loc piece loc1));
    assert_equal loc2 (Piece.Pieces.get_loc (Piece.Pieces.set_loc piece loc2))

  let basic_empty_check =
    basic_check empty_board_piece Inputs.empty_color Inputs.empty_location

  let basic_board1_check =
    basic_check board1_piece Inputs.board1_color Inputs.board1_location

  let empty_moves_check =
    assert_move_contents_equal Inputs.empty_possible_moves
      (Piece.Pieces.get_valid_moves board1_piece empty_board)

  let board1_moves_check =
    assert_move_contents_equal Inputs.board1_possible_moves
      (Piece.Pieces.get_valid_moves board1_piece board1)

  let tests =
    [
      "basic_empty_check" >:: basic_empty_check;
      "basic_board1_check" >:: basic_board1_check;
      "empty_board_moves_check" >:: empty_moves_check;
      "board1_moves_check" >:: board1_moves_check;
    ]
end
