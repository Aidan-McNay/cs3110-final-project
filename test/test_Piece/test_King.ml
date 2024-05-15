(* @author Aidan McNay (acm289) *)

open OUnit2

module BasicKingParams = struct
  let piece_type = Piece.Types.King
  let points = 200
end

module BasicKingTestInputs : Test_utils.PieceTestInputs = struct
  include BasicKingParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'D' 5
  let board = Test_utils.empty_board

  let possible_moves =
    Utils.Move.
      [
        [ Up ];
        [ Down ];
        [ Left ];
        [ Right ];
        [ Up; Right ];
        [ Up; Left ];
        [ Down; Right ];
        [ Down; Left ];
      ]
end

module EdgeOfBoardKingTestInputs : Test_utils.PieceTestInputs = struct
  include BasicKingParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'A' 8
  let board = Test_utils.empty_board
  let possible_moves = Utils.Move.[ [ Down ]; [ Right ]; [ Down; Right ] ]
end

module CaptureKingTestInputs : Test_utils.PieceTestInputs = struct
  include BasicKingParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'G' 5
  let board = Test_utils.board1

  let possible_moves =
    Utils.Move.
      [
        [ Up ];
        [ Down ];
        [ Left ];
        [ Right ];
        [ Up; Left ];
        [ Down; Right ];
        [ Down; Left ];
      ]
end

module BasicKingTester = Test_utils.PieceTester (BasicKingTestInputs)
module EdgeOfBoardKingTester = Test_utils.PieceTester (EdgeOfBoardKingTestInputs)
module CaptureKingTester = Test_utils.PieceTester (CaptureKingTestInputs)

let test_suite =
  "King Test Suite"
  >::: List.flatten
         [
           BasicKingTester.tests;
           CaptureKingTester.tests;
           EdgeOfBoardKingTester.tests;
         ]

let _ = run_test_tt_main test_suite
