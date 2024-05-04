(* @author Andro Janashia (aj454)*)

open OUnit2

let create_move_record piece start finish check captured_a_piece castle promoted
    =
  let start = Utils.Location.init_loc (fst start) (snd start) in
  let finish = Utils.Location.init_loc (fst finish) (snd finish) in
  Board.Move_record.gen_record piece start finish check captured_a_piece castle
    promoted

let create_piece piece_type =
  Piece.Pieces.init piece_type White (Utils.Location.init_loc 'e' 3)

let move_record_test_list =
  Piece.Types.
    [
      create_move_record (create_piece Pawn) ('e', 2) ('e', 3) false false false
        None;
      create_move_record (create_piece Bishop) ('c', 1) ('g', 5) false false
        false None;
      create_move_record (create_piece Queen) ('d', 1) ('c', 3) false false
        false None;
      create_move_record (create_piece Knight) ('b', 1) ('a', 3) false false
        false None;
      create_move_record (create_piece Rook) ('a', 1) ('a', 7) false true false
        None;
      create_move_record (create_piece Queen) ('d', 5) ('g', 3) true false false
        None;
      create_move_record (create_piece Pawn) ('h', 2) ('h', 4) true true false
        None;
      create_move_record (create_piece Pawn) ('f', 7) ('f', 8) false false false
        (Some Queen);
      create_move_record (create_piece Pawn) ('g', 7) ('g', 8) true false false
        (Some Knight);
      create_move_record (create_piece Pawn) ('a', 2) ('a', 1) false true false
        (Some Bishop);
      create_move_record (create_piece Pawn) ('b', 2) ('b', 1) true true false
        (Some Rook);
      create_move_record (create_piece King) ('e', 1) ('g', 1) false false true
        None;
      create_move_record (create_piece King) ('e', 1) ('c', 1) false false true
        None;
      create_move_record
        (Piece.Pieces.init King Black (Utils.Location.init_loc 'e' 8))
        ('e', 8) ('g', 8) false false true None;
      create_move_record
        (Piece.Pieces.init King Black (Utils.Location.init_loc 'e' 8))
        ('e', 8) ('c', 8) false false true None;
    ]

let tests =
  "algebraic notation test"
  >::: [
         ( "move record list check" >:: fun _ ->
           assert_equal
             [
               "e2-e3";
               "Bc1-g5";
               "Qd1-c3";
               "Nb1-a3";
               "Ra1xa7";
               "Qd5-g3+";
               "h2xh4+";
               "f7-f8Q";
               "g7-g8N+";
               "a2xa1B";
               "b2xb1R+";
               "0-0";
               "0-0-0";
               "0-0";
               "0-0-0";
             ]
             (Board.Alg_notation.alg_notation_move_history move_record_test_list)
         );
         ( "move record list to string test" >:: fun _ ->
           assert_equal
             "1. e2-e3  Bc1-g5\n\
              2. Qd1-c3  Nb1-a3\n\
              3. Ra1xa7  Qd5-g3+\n\
              4. h2xh4+  f7-f8Q\n\
              5. g7-g8N+  a2xa1B\n\
              6. b2xb1R+  0-0\n\
              7. 0-0-0  0-0\n\
              8. 0-0-0"
             (Board.Alg_notation.move_history_to_string
                (Board.Alg_notation.alg_notation_move_history
                   move_record_test_list)
                1) );
         ( "empty list to string test" >:: fun _ ->
           assert_equal ""
             (Board.Alg_notation.move_history_to_string
                (Board.Alg_notation.alg_notation_move_history [])
                1) );
         ( "one move record to string test" >:: fun _ ->
           assert_equal "1. e2-e3"
             Board.Alg_notation.(
               move_history_to_string
                 (alg_notation_move_history [ List.hd move_record_test_list ])
                 1) );
         ( "two move records to string test" >:: fun _ ->
           assert_equal "1. e2-e3  e2-e3"
             Board.Alg_notation.(
               move_history_to_string
                 (alg_notation_move_history
                    [
                      List.hd move_record_test_list;
                      List.hd move_record_test_list;
                    ])
                 1) );
       ]

let _ = run_test_tt_main tests
