(* @author Aidan McNay (acm289) *)

open OUnit2

module WhitePawnTestInputs : Test_utils.PieceTestInputs = struct
  let piece_type = Piece.Types.Pawn
  let points = 1
  let empty_color = Piece.Types.White
  let empty_location = Utils.Location.init_loc 'B' 2
  let empty_str_rep = "\u{2659}" (* White Pawn *)
  let empty_possible_moves = Utils.Move.[ [ Up ]; [ Up; Up ] ]
  let board1_color = Piece.Types.White
  let board1_str_rep = empty_str_rep
  let board1_location = Utils.Location.init_loc 'G' 6

  let board1_possible_moves =
    [ [ Utils.Move.Up; Utils.Move.Left ]; [ Utils.Move.Up; Utils.Move.Right ] ]
end

module PawnTester = Test_utils.PieceTester (WhitePawnTestInputs)

let test_suite = "Pawn Test Suite" >::: PawnTester.tests
let _ = run_test_tt_main test_suite
