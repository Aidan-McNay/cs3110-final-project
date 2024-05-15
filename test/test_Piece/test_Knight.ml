(* @author Aidan McNay (acm289) *)

open OUnit2

module BasicKnightParams = struct
  let piece_type = Piece.Types.Knight
  let points = 3
end

module BasicKnightTestInputs : Test_utils.PieceTestInputs = struct
  include BasicKnightParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'D' 5
  let board = Test_utils.empty_board

  let possible_moves =
    Utils.Move.
      [
        [ Up; Up; Left ];
        [ Up; Up; Right ];
        [ Down; Down; Left ];
        [ Down; Down; Right ];
        [ Left; Left; Up ];
        [ Left; Left; Down ];
        [ Right; Right; Up ];
        [ Right; Right; Down ];
      ]
end

module CaptureKnightTestInputs : Test_utils.PieceTestInputs = struct
  include BasicKnightParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'A' 4
  let board = Test_utils.board1

  let possible_moves =
    Utils.Move.
      [ [ Down; Down; Right ]; [ Down; Right; Right ]; [ Up; Up; Right ] ]
end

module BasicKnightTester = Test_utils.PieceTester (BasicKnightTestInputs)
module CaptureKnightTester = Test_utils.PieceTester (CaptureKnightTestInputs)

let test_suite =
  "Knight Test Suite"
  >::: List.flatten [ BasicKnightTester.tests; CaptureKnightTester.tests ]

let _ = run_test_tt_main test_suite
