(* @author Aidan McNay (acm289) *)

open OUnit2

module BasicRookParams = struct
  let piece_type = Piece.Types.Rook
  let points = 5
end

module BasicRookTestInputs : Test_utils.PieceTestInputs = struct
  include BasicRookParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'D' 5
  let board = Test_utils.empty_board

  let possible_moves =
    Utils.Move.
      [
        [ Left ];
        [ Left; Left ];
        [ Left; Left; Left ];
        [ Up ];
        [ Up; Up ];
        [ Up; Up; Up ];
        [ Right ];
        [ Right; Right ];
        [ Right; Right; Right ];
        [ Right; Right; Right; Right ];
        [ Down ];
        [ Down; Down ];
        [ Down; Down; Down ];
        [ Down; Down; Down; Down ];
      ]
end

module CaptureRookTestInputs : Test_utils.PieceTestInputs = struct
  include BasicRookParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'E' 4
  let board = Test_utils.board1

  let possible_moves =
    Utils.Move.
      [
        [ Left ];
        [ Right ];
        [ Up ];
        [ Up; Up ];
        [ Up; Up; Up ];
        [ Down ];
        [ Down; Down ];
        [ Down; Down; Down ];
      ]
end

module BasicRookTester = Test_utils.PieceTester (BasicRookTestInputs)
module CaptureRookTester = Test_utils.PieceTester (CaptureRookTestInputs)

let test_suite =
  "Rook Test Suite"
  >::: List.flatten [ BasicRookTester.tests; CaptureRookTester.tests ]

let _ = run_test_tt_main test_suite
