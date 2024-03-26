(* @author Aidan McNay (acm289) *)

open OUnit2

module BasicQueenParams = struct
  let piece_type = Piece.Types.Queen
  let points = 9
  let white_str_rep = "\u{2655}"
  let black_str_rep = "\u{265B}"
end

module BasicQueenTestInputs : Test_utils.PieceTestInputs = struct
  include BasicQueenParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'D' 5
  let str_rep = white_str_rep
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
        [ Down; Left ];
        [ Down; Left; Down; Left ];
        [ Down; Left; Down; Left; Down; Left ];
        [ Up; Right ];
        [ Up; Right; Up; Right ];
        [ Up; Right; Up; Right; Up; Right ];
        [ Up; Left ];
        [ Up; Left; Up; Left ];
        [ Up; Left; Up; Left; Up; Left ];
        [ Down; Right ];
        [ Down; Right; Down; Right ];
        [ Down; Right; Down; Right; Down; Right ];
        [ Down; Right; Down; Right; Down; Right; Down; Right ];
      ]
end

module CaptureQueenTestInputs : Test_utils.PieceTestInputs = struct
  include BasicQueenParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'D' 4
  let str_rep = black_str_rep
  let board = Test_utils.board1

  let possible_moves =
    Utils.Move.
      [
        [ Right ];
        [ Right; Right ];
        [ Down ];
        [ Down; Down ];
        [ Down; Right ];
        [ Down; Right; Down; Right ];
        [ Down; Right; Down; Right; Down; Right ];
        [ Down; Left ];
        [ Up; Left ];
        [ Up; Right ];
        [ Up; Right; Up; Right ];
      ]
end

module BasicQueenTester = Test_utils.PieceTester (BasicQueenTestInputs)
module CaptureQueenTester = Test_utils.PieceTester (CaptureQueenTestInputs)

let test_suite =
  "Queen Test Suite"
  >::: List.flatten [ BasicQueenTester.tests; CaptureQueenTester.tests ]

let _ = run_test_tt_main test_suite
