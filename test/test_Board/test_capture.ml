(* @author Aidan McNay (acm289) *)

open OUnit2

(** [mk_capture_record piece_type color start finish] is a record outcome for a
    move of a [piece_type] piece of color [color] from [start] to [finish] on
    the intermediate board, capturing a piece. *)
let mk_capture_record piece_type color start finish =
  Test_utils.Record
    (Board.Move_record.gen_record piece_type color start finish false true false
       None false false)

(** [BlackCaptureTest] is a module for a valid capture of a white piece by black *)
module BlackCaptureTest : Test_utils.BoardTest = struct
  (** [capture_start] is the starting location of the capturing piece. *)
  let capture_start = Utils.Location.init_loc 'H' 6

  (** [capture_finish] is the finishing location of the capturing piece. *)
  let capture_finish = Utils.Location.init_loc 'H' 4

  let board_state = Test_utils.intermediate_board
  let history = []

  let capture_record =
    mk_capture_record Piece.Types.Rook Piece.Types.Black capture_start
      capture_finish

  let moves_to_test =
    [
      ( "black-capture",
        Piece.Types.Black,
        capture_start,
        capture_finish,
        capture_record );
    ]
end

(** [WhiteCaptureTest] is a module for a valid capture of a black piece by white *)
module WhiteCaptureTest : Test_utils.BoardTest = struct
  (** [capture_start] is the starting location of the capturing piece. *)
  let capture_start = Utils.Location.init_loc 'B' 1

  (** [capture_finish] is the finishing location of the capturing piece. *)
  let capture_finish = Utils.Location.init_loc 'H' 7

  let board_state = Test_utils.intermediate_board
  let history = []

  let capture_record =
    mk_capture_record Piece.Types.Bishop Piece.Types.White capture_start
      capture_finish

  let moves_to_test =
    [
      ( "white-capture",
        Piece.Types.White,
        capture_start,
        capture_finish,
        capture_record );
    ]
end

let test_modules : (module Test_utils.BoardTest) list =
  [ (module BlackCaptureTest); (module WhiteCaptureTest) ]

let tests =
  let get_tests m =
    let module S = (val m : Test_utils.BoardTest) in
    let module T = Test_utils.BoardTester (S) in
    T.tests
  in
  List.flatten (List.map get_tests test_modules)

let test_suite = "Capture Test Suite" >::: tests
let _ = run_test_tt_main test_suite
