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

  val location : Utils.Location.t
  (** [location] is the location we're testing. *)

  val color : Piece.Types.color
  (** [color] is the color of the piece we're testing. *)

  val str_rep : string
  (** [str_rep] is the string representation of the piece we're testing *)

  val board : Piece.Pieces.t list
  (** [board] is the other pieces on the board, besides the piece we're testing. *)

  val possible_moves : Utils.Move.moves list
  (** [possible_moves] is the possible moves the piece should have from
      [location] on [board]. *)
end

(** [moves_equal move1 move2] is whether [move1] and [move2] represent the same
    move, even if in a different order. *)
let moves_equal moves1 moves2 =
  let moves1_moves_in_moves2 lst1 lst2 =
    List.for_all (fun el -> List.exists (fun x -> x = el) lst2) lst1
  in
  moves1_moves_in_moves2 moves1 moves2 && moves1_moves_in_moves2 moves2 moves1

(** [int_of_move move] is a different value for each [move], to allow for
    sorting. *)
let int_of_move = function
  | Utils.Move.Up -> 4
  | Utils.Move.Down -> 3
  | Utils.Move.Left -> 2
  | Utils.Move.Right -> 1

(** [sort_moves moves] is the sorted version of [moves]. *)
let sort_moves =
  List.sort (fun move1 move2 -> int_of_move move1 - int_of_move move2)

(** [compare_moves moves1 moves2] is [1] if [move1] comes before [move2], [-1]
    if [move2] comes before [move1], and [0] if they are the same. *)
let rec compare_moves moves1 moves2 =
  match (moves1, moves2) with
  | [], [] -> 0
  | [], _ :: _ -> 1
  | _ :: _, [] -> -1
  | x :: xs, y :: ys ->
      let comparison = int_of_move x - int_of_move y in
      if comparison > 0 then 1
      else if comparison < 0 then -1
      else compare_moves xs ys

(** [moves_list_printer moves_list] is the string representation of
    [moves_list]. *)
let moves_list_printer moves_list =
  let sorted_moves_list =
    List.sort compare_moves (List.map sort_moves moves_list)
  in
  let string_moves_list =
    List.map (List.map Utils.Move.to_string) sorted_moves_list
  in
  let moves_string_list = List.map (String.concat "-") string_moves_list in
  String.concat " " moves_string_list

(** [moves_contents_equal lst1 lst2] is whether [lst1] and [lst2] have the same
    contents, even if in a different order. *)
let moves_contents_equal lst1 lst2 =
  let str_lst1 = moves_list_printer lst1 in
  let str_lst2 = moves_list_printer lst2 in
  str_lst1 = str_lst2

(** A functor for testing a piece, based on test inputs of type
    [PieceTestInputs]. *)
module PieceTester (Inputs : PieceTestInputs) = struct
  let piece = Inputs.(Piece.Pieces.init piece_type color location)

  let basic_check _ =
    assert_equal Inputs.piece_type
      (Piece.Pieces.get_type piece)
      ~printer:Piece.Types.str_of_type;
    assert_equal Inputs.color
      (Piece.Pieces.get_color piece)
      ~printer:Piece.Types.str_of_color;
    assert_equal Inputs.location
      (Piece.Pieces.get_loc piece)
      ~printer:Utils.Location.str_of_loc;
    assert_equal Inputs.points
      (Piece.Pieces.get_points piece)
      ~printer:string_of_int;
    assert_equal Inputs.str_rep (Piece.Pieces.to_string piece) ~printer:Fun.id;
    let loc1 = Utils.Location.init_loc 'B' 8 in
    let loc2 = Utils.Location.init_loc 'G' 3 in
    assert_equal loc1
      (Piece.Pieces.get_loc (Piece.Pieces.set_loc piece loc1))
      ~printer:Utils.Location.str_of_loc;
    assert_equal loc2
      (Piece.Pieces.get_loc (Piece.Pieces.set_loc piece loc2))
      ~printer:Utils.Location.str_of_loc

  let moves_check _ =
    assert_equal
      (List.sort compare_moves (List.map sort_moves Inputs.possible_moves))
      (Piece.Pieces.get_valid_moves piece Inputs.board)
      ~cmp:moves_contents_equal ~printer:moves_list_printer

  let tests = [ "basic_check" >:: basic_check; "moves_check" >:: moves_check ]
end
