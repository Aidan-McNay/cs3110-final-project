(* @author Aidan McNay (acm289) *)

open OUnit2

(** [check_board] is a board state with many checks possible, representing a
    game part-way done. It can be viewed at https://lichess.org/study/M508si0T *)
let check_board =
  Piece.Pieces.
    [
      init King White (Utils.Location.init_loc 'H' 1);
      init Knight White (Utils.Location.init_loc 'D' 1);
      init Queen Black (Utils.Location.init_loc 'B' 1);
      init Rook White (Utils.Location.init_loc 'A' 1);
      init Pawn White (Utils.Location.init_loc 'H' 2);
      init Queen White (Utils.Location.init_loc 'G' 2);
      init Pawn Black (Utils.Location.init_loc 'F' 2);
      init Pawn White (Utils.Location.init_loc 'D' 2);
      init Pawn White (Utils.Location.init_loc 'B' 2);
      init Pawn Black (Utils.Location.init_loc 'H' 3);
      init Pawn Black (Utils.Location.init_loc 'G' 3);
      init Pawn White (Utils.Location.init_loc 'F' 3);
      init Bishop White (Utils.Location.init_loc 'D' 3);
      init King Black (Utils.Location.init_loc 'B' 3);
      init Pawn White (Utils.Location.init_loc 'H' 4);
      init Rook Black (Utils.Location.init_loc 'F' 4);
      init Knight Black (Utils.Location.init_loc 'E' 4);
      init Rook White (Utils.Location.init_loc 'D' 4);
      init Pawn Black (Utils.Location.init_loc 'C' 4);
      init Pawn White (Utils.Location.init_loc 'D' 5);
      init Rook Black (Utils.Location.init_loc 'H' 6);
      init Pawn White (Utils.Location.init_loc 'G' 6);
      init Pawn White (Utils.Location.init_loc 'F' 6);
      init Knight Black (Utils.Location.init_loc 'E' 6);
      init Pawn Black (Utils.Location.init_loc 'C' 6);
      init Bishop Black (Utils.Location.init_loc 'B' 6);
      init Pawn Black (Utils.Location.init_loc 'H' 7);
      init Pawn Black (Utils.Location.init_loc 'F' 7);
      init Bishop White (Utils.Location.init_loc 'E' 7);
      init Knight White (Utils.Location.init_loc 'D' 7);
      init Pawn Black (Utils.Location.init_loc 'A' 7);
      init Bishop Black (Utils.Location.init_loc 'C' 8);
    ]

(** [mk_check_record piece_type color start finish is_capture, is_checkmate] is
    a record outcome for a move of a [piece_type] piece of color [color] from
    [start] to [finish] on the intermediate, checking the opposing king.
    [is_capture] indicates whether a piece was captured in the process, and
    [is_checkmate] indicates whether this is a checkmate or not. *)
let mk_check_record piece_type color start finish is_capture is_checkmate =
  Test_utils.Record
    (Board.Move_record.gen_record piece_type color start finish true is_capture
       false None is_checkmate false)

(** [BlackCheckTest] is a module for black putting white in check *)
module BlackCheckTest : Test_utils.BoardTest = struct
  (** [check_start] is the starting location of the checking piece. *)
  let check_start = Utils.Location.init_loc 'B' 1

  (** [check_finish] is the finishing location of the checking piece. *)
  let check_finish = Utils.Location.init_loc 'D' 1

  let board_state = check_board
  let history = []

  let check_record =
    mk_check_record Piece.Types.Queen Piece.Types.Black check_start check_finish
      true false

  let moves_to_test =
    [
      ("black-check", Piece.Types.Black, check_start, check_finish, check_record);
    ]
end

(** [WhiteCheckTest] is a module for white putting black in check *)
module WhiteCheckTest : Test_utils.BoardTest = struct
  (** [check_start] is the starting location of the checking piece. *)
  let check_start = Utils.Location.init_loc 'D' 3

  (** [check_finish] is the finishing location of the checking piece. *)
  let check_finish = Utils.Location.init_loc 'C' 2

  let board_state = check_board
  let history = []

  let check_record =
    mk_check_record Piece.Types.Bishop Piece.Types.White check_start
      check_finish false false

  let moves_to_test =
    [
      ("white-check", Piece.Types.White, check_start, check_finish, check_record);
    ]
end

(** [WhiteCheckmateTest] is a module for white putting black in checkmate *)
module WhiteCheckmateTest : Test_utils.BoardTest = struct
  (** [check_start] is the starting location of the checking piece. *)
  let check_start = Utils.Location.init_loc 'A' 1

  (** [check_finish] is the finishing location of the checking piece. *)
  let check_finish = Utils.Location.init_loc 'A' 3

  let board_state = check_board
  let history = []

  let check_record =
    mk_check_record Piece.Types.Rook Piece.Types.White check_start check_finish
      false true

  let moves_to_test =
    [
      ( "white-checkmate",
        Piece.Types.White,
        check_start,
        check_finish,
        check_record );
    ]
end

let test_modules : (module Test_utils.BoardTest) list =
  [
    (module BlackCheckTest);
    (module WhiteCheckTest);
    (module WhiteCheckmateTest);
  ]

let tests =
  let get_tests m =
    let module S = (val m : Test_utils.BoardTest) in
    let module T = Test_utils.BoardTester (S) in
    T.tests
  in
  List.flatten (List.map get_tests test_modules)

let test_suite = "Check Test Suite" >::: tests
let _ = run_test_tt_main test_suite
