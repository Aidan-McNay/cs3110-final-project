(* @author Aidan McNay (acm289) *)
(* This represents simulating a "realistic" game, checking the board along the
   way. This particular game is a Fool's Mate in response to Bird's Opening,
   seen here: https://www.chess.com/article/view/fastest-chess-checkmates *)

open OUnit2

let initial_board = Board.Chessboard.initial

(** [initial_tests] verify the starting state of our initial board. *)
let initial_tests =
  [
    ( "initial-white-not-in-checkmate" >:: fun _ ->
      assert_equal false
        (Board.Chessboard.in_checkmate Piece.Types.White initial_board) );
    ( "initial-black-not-in-checkmate" >:: fun _ ->
      assert_equal false
        (Board.Chessboard.in_checkmate Piece.Types.Black initial_board) );
    ( "initial-no-last-move" >:: fun _ ->
      assert_raises Board.Chessboard.No_moves_made (fun () ->
          Board.Chessboard.last_move initial_board) );
    ( "initial-no-move-history" >:: fun _ ->
      assert_equal [] (Board.Chessboard.move_history initial_board) );
    ( "initial-no-notation" >:: fun _ ->
      assert_equal [] (Board.Chessboard.get_rev_alg_notation initial_board) );
  ]

(** [records_to_apply] is the records indicating the moves made for Fool's Mate. *)
let records_to_apply =
  Piece.Types.
    [
      Test_utils.test_record Pawn White ('F', 2) ('F', 4);
      Test_utils.test_record Pawn Black ('E', 7) ('E', 5);
      Test_utils.test_record Pawn White ('F', 4) ('E', 5) ~capture:true;
      Test_utils.test_record Pawn Black ('D', 7) ('D', 6);
      Test_utils.test_record Pawn White ('E', 5) ('D', 6) ~capture:true;
      Test_utils.test_record Bishop Black ('F', 8) ('D', 6) ~capture:true;
      Test_utils.test_record Knight White ('B', 1) ('C', 3);
      Test_utils.test_record Queen Black ('D', 8) ('H', 4) ~is_check:true;
      Test_utils.test_record Pawn White ('G', 2) ('G', 3);
      Test_utils.test_record Queen Black ('H', 4) ('G', 3) ~is_check:true
        ~capture:true;
      Test_utils.test_record Pawn White ('H', 2) ('G', 3) ~capture:true;
      Test_utils.test_record Bishop Black ('D', 6) ('G', 3) ~capture:true
        ~is_check:true ~checkmate:true;
    ]

(** [apply_move board record] is [board] after the move indicated by [record]
    has been made on it. *)
let apply_move board record =
  let color = Board.Move_record.get_color record in
  let start = Board.Move_record.get_start record in
  let finish = Board.Move_record.get_finish record in
  Board.Chessboard.move_piece board color start finish

(** [ending_board] is the board state after the Fool's Mate game has been
    played. *)
let ending_board = List.fold_left apply_move initial_board records_to_apply

(** [record_tests] verify the record history in our ending board. *)
let record_tests =
  List.map2
    (fun rec1 rec2 ->
      "move-record-test" >:: fun _ ->
      assert_equal rec1 rec2 ~cmp:Test_utils.compare_records
        ~printer:(Board.Alg_notation.move_record_to_alg_notation []))
    records_to_apply
    (List.rev (Board.Chessboard.move_history ending_board))

(** [exp_alg_not] is our expected algebraic notation for our board. *)
let exp_alg_not =
  List.map (Board.Alg_notation.move_record_to_alg_notation []) records_to_apply
  @ [ "0-1" ]

(** [alg_not_tests] verify the algebraic notation of our ending board. *)
let alg_not_tests =
  List.map2
    (fun not1 not2 ->
      "algebraic-notation-test" >:: fun _ ->
      assert_equal not1 not2 ~printer:Fun.id)
    exp_alg_not
    (Board.Chessboard.get_rev_alg_notation ending_board)

(** [ending_tests] verify the remaining state of our ending board. *)
let ending_tests =
  [
    ( "ending-white-in-checkmate" >:: fun _ ->
      assert_equal true
        (Board.Chessboard.in_checkmate Piece.Types.White ending_board) );
    ( "ending-black-not-in-checkmate" >:: fun _ ->
      assert_equal false
        (Board.Chessboard.in_checkmate Piece.Types.Black ending_board) );
    ( "ending-last-move" >:: fun _ ->
      assert_equal
        (List.hd (List.rev records_to_apply))
        (Board.Chessboard.last_move ending_board)
        ~cmp:Test_utils.compare_records );
  ]

let test_suite =
  "Realistic Test Suite"
  >::: initial_tests @ record_tests @ alg_not_tests @ ending_tests

let _ = run_test_tt_main test_suite
