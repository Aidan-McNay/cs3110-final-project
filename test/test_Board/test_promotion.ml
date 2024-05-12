(* @author Aidan McNay (acm289) *)

open OUnit2

(** [promote_board] is a board state with multiple promotions possible *)
let promote_board =
  Piece.Pieces.
    [
      init King White (Utils.Location.init_loc 'E' 1);
      init King Black (Utils.Location.init_loc 'E' 7);
      init Pawn Black (Utils.Location.init_loc 'A' 2);
      init Pawn White (Utils.Location.init_loc 'B' 7);
      init Rook Black (Utils.Location.init_loc 'H' 2);
    ]

(** [white_promote_test] is the test for when write promotes on [promote_board]
    from B7 to B8. *)
let white_promote_test =
  let promote_start = Utils.Location.init_loc 'B' 7 in
  let promote_finish = Utils.Location.init_loc 'B' 8 in
  let promote_record =
    Piece.Types.(
      Board.Move_record.gen_record Pawn White promote_start promote_finish false
        false false (Some Queen) false false)
  in
  ( "white-promotion",
    Piece.Types.White,
    promote_start,
    promote_finish,
    Test_utils.Record promote_record )

(** [black_promote_test] is the test for when write promotes on [promote_board]
    from A2 to A1. *)
let black_promote_test =
  let promote_start = Utils.Location.init_loc 'A' 2 in
  let promote_finish = Utils.Location.init_loc 'A' 1 in
  let promote_record =
    Piece.Types.(
      Board.Move_record.gen_record Pawn Black promote_start promote_finish true
        false false (Some Queen) true false)
  in
  ( "black-promotion",
    Piece.Types.Black,
    promote_start,
    promote_finish,
    Test_utils.Record promote_record )

(** [PromotionTest] is a module for testing promotion on [promote_board] *)
module PromotionTest : Test_utils.BoardTest = struct
  (** [check_start] is the starting location of the checking piece. *)

  let board_state = promote_board
  let history = []
  let moves_to_test = [ white_promote_test; black_promote_test ]
end

module PromotionTester = Test_utils.BoardTester (PromotionTest)

let test_suite = "Promotion Test Suite" >::: PromotionTester.tests
let _ = run_test_tt_main test_suite
