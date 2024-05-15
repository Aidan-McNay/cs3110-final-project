(* @author Aidan McNay (acm289) *)

open OUnit2

module BasicPawnParams = struct
  let piece_type = Piece.Types.Pawn
  let points = 1
end

module BasicPawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'B' 4
  let board = Test_utils.empty_board
  let possible_moves = Utils.Move.[ [ Up ] ]
end

module DoubleMoveWhitePawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'C' 2
  let board = Test_utils.empty_board
  let possible_moves = Utils.Move.[ [ Up ]; [ Up; Up ] ]
end

module DoubleMoveBlackPawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'H' 7
  let board = Test_utils.empty_board
  let possible_moves = Utils.Move.[ [ Down ]; [ Down; Down ] ]
end

module EndOfBoardPawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'F' 8
  let board = Test_utils.empty_board
  let possible_moves = []
end

module CapturePawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'C' 3
  let board = Test_utils.board1
  let possible_moves = Utils.Move.[ [ Down ]; [ Down; Right ] ]
end

module ObstructFrontMovePawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.White
  let location = Utils.Location.init_loc 'E' 2

  let board =
    Piece.Pieces.(init Pawn White (Utils.Location.init_loc 'E' 3))
    :: Test_utils.board1

  let possible_moves = []
end

module ObstructDoubleMovePawnTestInputs : Test_utils.PieceTestInputs = struct
  include BasicPawnParams

  let color = Piece.Types.Black
  let location = Utils.Location.init_loc 'D' 7
  let board = Test_utils.board1
  let possible_moves = Utils.Move.[ [ Down ] ]
end

module BasicPawnTester = Test_utils.PieceTester (BasicPawnTestInputs)

module DoubleMoveWhitePawnTester =
  Test_utils.PieceTester (DoubleMoveWhitePawnTestInputs)

module DoubleMoveBlackPawnTester =
  Test_utils.PieceTester (DoubleMoveBlackPawnTestInputs)

module EndOfBoardPawnTester = Test_utils.PieceTester (EndOfBoardPawnTestInputs)
module CapturePawnTester = Test_utils.PieceTester (CapturePawnTestInputs)

module ObstructFrontMovePawnTester =
  Test_utils.PieceTester (ObstructFrontMovePawnTestInputs)

module ObstructDoubleMovePawnTester =
  Test_utils.PieceTester (ObstructDoubleMovePawnTestInputs)

let test_suite =
  "Pawn Test Suite"
  >::: List.flatten
         [
           BasicPawnTester.tests;
           DoubleMoveWhitePawnTester.tests;
           DoubleMoveBlackPawnTester.tests;
           EndOfBoardPawnTester.tests;
           CapturePawnTester.tests;
           ObstructFrontMovePawnTester.tests;
           ObstructDoubleMovePawnTester.tests;
         ]

let _ = run_test_tt_main test_suite
