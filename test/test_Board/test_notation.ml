(* @author Andro Janashia (aj454) *)

open OUnit2

(** [test_from_input name record ?ambig expected_notation] is a test named
    [name] checking that the notation for [record] is the same as
    [expected_notation]. [ambig] are the other pieces that could take the ending
    location. *)
let test_from_input name record ?(ambig = []) expected_notation =
  let notation = Board.Alg_notation.move_record_to_alg_notation ambig record in
  name >:: fun _ -> assert_equal expected_notation notation ~printer:Fun.id

let test_to_string name alg_list expected_string =
  let notation = Board.Alg_notation.move_history_to_algformat alg_list 1 in
  name >:: fun _ -> assert_equal expected_string notation ~printer:Fun.id

(** [tests] are the tests to run. *)
let tests =
  Piece.Types.
    [
      test_from_input "pawn-move"
        (Test_utils.test_record Pawn White ('E', 2) ('E', 4))
        "e4";
      test_from_input "pawn-promotion"
        (Test_utils.test_record Pawn White ('A', 7) ('A', 8)
           ~promoted:Piece.Types.Queen)
        "a8=Q";
      test_from_input "pawn-promotion+check"
        (Test_utils.test_record Pawn White ('A', 7) ('A', 8)
           ~promoted:Piece.Types.Queen ~is_check:true)
        "a8=Q+";
      test_from_input "pawn-promotion+checkmate"
        (Test_utils.test_record Pawn White ('B', 7) ('B', 8)
           ~promoted:Piece.Types.Queen ~checkmate:true ~is_check:true)
        "b8=Q#";
      test_from_input "pawn-promotion+capture+checkmate"
        (Test_utils.test_record Pawn White ('B', 7) ('C', 8)
           ~promoted:Piece.Types.Queen ~capture:true ~checkmate:true
           ~is_check:true)
        "bxc8=Q#";
      test_from_input "pawn-enpassant"
        (Test_utils.test_record Pawn White ('C', 5) ('D', 6) ~capture:true
           ~en_passant:true)
        "cxd6 e.p.";
      test_from_input "pawn-enpassant+check"
        (Test_utils.test_record Pawn White ('C', 5) ('D', 6) ~capture:true
           ~en_passant:true ~is_check:true)
        "cxd6+ e.p.";
      test_from_input "pawn-enpassant+checkmate"
        (Test_utils.test_record Pawn White ('E', 5) ('F', 6) ~capture:true
           ~en_passant:true ~is_check:true ~checkmate:true)
        "exf6# e.p.";
      test_from_input "king-move"
        (Test_utils.test_record King Black ('E', 5) ('F', 6) ~capture:true)
        "Kxf6";
      test_from_input "file-disambiguation"
        (Test_utils.test_record Rook Black ('D', 8) ('F', 8))
        ~ambig:[ Piece.Pieces.init Rook Black (Utils.Location.init_loc 'H' 8) ]
        "Rdf8";
      test_from_input "rank-disambiguation"
        (Test_utils.test_record Rook White ('A', 1) ('A', 3))
        ~ambig:[ Piece.Pieces.init Rook White (Utils.Location.init_loc 'A' 5) ]
        "R1a3";
      test_from_input "rank-file-disambiguation"
        (Test_utils.test_record Queen White ('H', 4) ('E', 1))
        ~ambig:
          [
            Piece.Pieces.init Queen White (Utils.Location.init_loc 'E' 4);
            Piece.Pieces.init Queen White (Utils.Location.init_loc 'H' 1);
          ]
        "Qh4e1";
      test_to_string "real-game-history" Test_utils.sample_game_history
        "1. e4 e5 2. Nf3 Nc6 3. Bc4 Bc5 4. c3 Nf6 5. d3 O-O 6. O-O d5 7. exd5 \
         Nxd5 8. a4 a6 9. Re1 Bg4 10. Nbd2 Kh8 11. h3 Bh5 12. Ne4 Ba7 13. Ng3 \
         Bg6 14. Nxe5 Nxe5 15. Rxe5 Nb6 16. Qf3 c6 17. Bf4 Bb8 18. Ree1 Nxc4 \
         19. dxc4 Qh4 20. Ne2 Ba7 21. Bd6 Rfe8 22. Nf4 Bc2 23. c5 a5 24. Re2 \
         Bb3 25. Ra3 1-0";
      test_to_string "empty-history" [] "";
      test_to_string "one-move-history" [ "e3" ] "1. e3 ";
      test_to_string "white-win" [ "e3"; "f4"; "1-0" ] "1. e3 f4 1-0";
      test_to_string "black-win" [ "e3"; "f4"; "0-1" ] "1. e3 f4 0-1";
    ]

let _ = run_test_tt_main ("Notation Test Suite" >::: tests)
