(* @author Aidan McNay (acm289) *)

open OUnit2

module BasicBishopParams = struct
  let piece_type = Piece.Types.Bishop
  let points = 3
end

module BasicBishopTestInputs : Test_utils.PieceTestInputs = struct
  include BasicBishopParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'D' 5
  let board = Test_utils.empty_board

  let possible_moves =
    Utils.Move.
      [
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

module CaptureBishopTestInputs : Test_utils.PieceTestInputs = struct
  include BasicBishopParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'D' 3
  let board = Test_utils.board1

  let possible_moves =
    Utils.Move.
      [
        [ Down; Right ];
        [ Down; Right; Down; Right ];
        [ Down; Left ];
        [ Down; Left; Down; Left ];
        [ Up; Right ];
        [ Up; Right; Up; Right ];
        [ Up; Right; Up; Right; Up; Right ];
      ]
end

module BasicBishopTester = Test_utils.PieceTester (BasicBishopTestInputs)
module CaptureBishopTester = Test_utils.PieceTester (CaptureBishopTestInputs)

let test_suite =
  "Bishop Test Suite"
  >::: List.flatten [ BasicBishopTester.tests; CaptureBishopTester.tests ]

let _ = run_test_tt_main test_suite
