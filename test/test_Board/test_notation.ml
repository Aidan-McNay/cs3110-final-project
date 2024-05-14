(* @author Andro Janashia (aj454) *)

open OUnit2

(** [test_from_input name record ?ambig expected_notation] is a test named
    [name] checking that the notation for [record] is the same as
    [expected_notation]. [ambig] are the other pieces that could take the ending
    location. *)
let test_from_input name record ?(ambig = []) expected_notation =
  let notation = Board.Alg_notation.move_record_to_alg_notation ambig record in
  name >:: fun _ -> assert_equal expected_notation notation ~printer:Fun.id

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
    ]

(* let move_record_test_list = Piece.Types. [ create_move_record (Pawn, White)
   ('d', 5) ('e', 6) true true false None true true; create_move_record (Pawn,
   Black) ('b', 4) ('a', 3) false true false None false true; create_move_record
   (Pawn, White) ('e', 2) ('e', 3) false false false None false false;
   create_move_record (Bishop, White) ('c', 1) ('g', 5) false false false None
   false false; create_move_record (Queen, Black) ('d', 1) ('c', 3) false false
   false None false false; create_move_record (Knight, White) ('b', 1) ('a', 3)
   false false false None false false; create_move_record (Rook, White) ('a', 1)
   ('a', 7) false true false None false false; create_move_record (Queen, White)
   ('d', 5) ('g', 3) true false false None false false; create_move_record
   (Pawn, White) ('h', 2) ('g', 3) true true false None false false;
   create_move_record (Pawn, White) ('f', 7) ('f', 8) false false false (Some
   Queen) false false; create_move_record (Pawn, White) ('g', 7) ('g', 8) true
   false false (Some Knight) false false; create_move_record (Pawn, White) ('a',
   2) ('b', 1) false true false (Some Bishop) false false; create_move_record
   (Pawn, White) ('c', 2) ('b', 1) true true false (Some Rook) false false;
   create_move_record (King, White) ('e', 1) ('g', 1) false false true None
   false false; create_move_record (King, White) ('e', 1) ('c', 1) false false
   true None false false; create_move_record (King, Black) ('e', 8) ('g', 8)
   false false true None false false; create_move_record (King, Black) ('e', 8)
   ('c', 8) false false true None false false; ]

   let tests = "algebraic notation test" >::: [ ( "move record list check" >::
   fun _ -> assert_equal [ "d5xe6+"; "b4xa3"; "e2-e3"; "Bc1-g5"; "Qd1-c3";
   "Nb1-a3"; "Ra1xa7"; "Qd5-g3+"; "h2xg3+"; "f7-f8Q"; "g7-g8N+"; "a2xb1B";
   "c2xb1R+"; "0-0"; "0-0-0"; "0-0"; "0-0-0"; ] (List.map
   Board.Alg_notation.move_record_to_longalgnotation move_record_test_list) ); (
   "move record list to string test" >:: fun _ -> assert_equal "1,0-0-0,0-0\n\
   2,0-0-0,0-0\n\ \ 3,cxb1R+,axb1B\n\ \ 4,g8N+,f8Q\n\ \ 5,hxg3+,Qg3+\n\ \
   6,Rxa7,Na3\n\ 7,Qc3,Bg5\n\ \ 8,e3,bxa3 e.p.\n\ \ 9,dxe6# e.p.,"
   Board.Alg_notation.( move_history_to_algformat (List.rev (List.map
   (move_record_to_alg_notation []) move_record_test_list)) 1) ); ( "empty list
   to string test" >:: fun _ -> assert_equal ""
   (Board.Alg_notation.move_history_to_algformat [] 1) ); ( "one\n move record
   to string test" >:: fun _ -> assert_equal "1,dxe6# e.p.,"
   Board.Alg_notation.( move_history_to_algformat [ move_record_to_alg_notation
   [] (List.hd move_record_test_list); ] 1) ); ( "two move records to string\n
   test" >:: fun _ -> assert_equal "1,dxe6# e.p.,dxe6# e.p."
   Board.Alg_notation.( move_history_to_algformat [ move_record_to_alg_notation
   [] (List.hd move_record_test_list); move_record_to_alg_notation [] (List.hd
   move_record_test_list); ] 1) ); ] *)

let _ = run_test_tt_main ("Notation Test Suite" >::: tests)
