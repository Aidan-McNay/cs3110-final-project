(* @author Aidan McNay (acm289) *)

open OUnit2

(** [mk_basic_record piece_type color start finish] is a record outcome for a
    basic move of a [piece_type] piece of color [color] from [start] to [finish]
    on the initial board. *)
let mk_basic_record piece_type color start finish =
  let alg_not =
    Board.Alg_notation.move_record_to_alg_notation [] piece_type start finish
      false false None false
  in
  Test_utils.Record
    (Board.Move_record.gen_record piece_type color start finish false false
       false None false alg_not)

type piece_move =
  string
  * Piece.Types.piece_type
  * Piece.Types.color
  * Utils.Location.t
  * Utils.Location.t
(** A type to represent a test of a specific piece movement. An entry
    [(name, piece_type, color, start, finish)] indicates a test named [name]
    that tries to move a piece of type [piece_type] and color [color] from
    [start] to [finish] on the initial board. *)

(** [valid_moves] is a list of valid piece movements on the initial board. *)
let valid_moves : piece_move list =
  Piece.Types.
    [
      ( "pawn-single-move",
        Pawn,
        White,
        Utils.Location.init_loc 'B' 2,
        Utils.Location.init_loc 'B' 3 );
      ( "pawn-double-move",
        Pawn,
        Black,
        Utils.Location.init_loc 'G' 7,
        Utils.Location.init_loc 'G' 5 );
      ( "knight-move",
        Knight,
        White,
        Utils.Location.init_loc 'B' 1,
        Utils.Location.init_loc 'C' 3 );
    ]

(** [valid_move_to_test entry] turns a piece movement [entry] into a test of a
    valid move on the initial board. *)
let valid_move_to_test ((name, piece_type, color, start, finish) : piece_move) :
    Test_utils.test =
  (name, color, start, finish, mk_basic_record piece_type color start finish)

(** [invalid_moves] is a list of invalid piece movements on the initial board. *)
let invalid_moves : piece_move list =
  Piece.Types.
    [
      ( "pawn-triple-move",
        Pawn,
        White,
        Utils.Location.init_loc 'B' 2,
        Utils.Location.init_loc 'B' 5 );
      ( "move-through-piece",
        Queen,
        Black,
        Utils.Location.init_loc 'D' 8,
        Utils.Location.init_loc 'D' 1 );
      ( "move-other-color",
        Pawn,
        Black,
        Utils.Location.init_loc 'E' 2,
        Utils.Location.init_loc 'E' 3 );
      ( "no-piece-there",
        Pawn,
        White,
        Utils.Location.init_loc 'C' 3,
        Utils.Location.init_loc 'C' 4 );
    ]

(** [invalid_move_to_test entry] turns a piece movement [entry] into a test of
    an invalid move on the initial board. *)
let invalid_move_to_test ((name, _, color, start, finish) : piece_move) :
    Test_utils.test =
  (name, color, start, finish, Test_utils.Invalid)

(** A module for specifying basic movement tests on an initial board. *)
module BasicBoardTest : Test_utils.BoardTest = struct
  let board_state = Piece.Pieces.start_state
  let history = []
  let valid_tests = List.map valid_move_to_test valid_moves
  let invalid_tests = List.map invalid_move_to_test invalid_moves
  let moves_to_test = List.flatten [ valid_tests; invalid_tests ]
end

module BasicBoardTester = Test_utils.BoardTester (BasicBoardTest)
(** A module for testing our basic tests. *)

let test_suite = "Basic Board Test Suite" >::: BasicBoardTester.tests
let _ = run_test_tt_main test_suite
