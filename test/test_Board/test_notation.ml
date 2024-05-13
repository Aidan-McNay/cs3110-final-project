(* @author Andro Janashia (aj454) *)

open OUnit2

(** [test_record ?is_check ?capture ?castle ?promoted ?checkmate ?en_passant piece_type color (col1, row1) (col2, row2)]
    is the record for moving a piece of type [piece_type] and color [color] from
    the location represented by [(col1, row1)] to the location represented by
    [(col2, row2)]. The optional arguments [is_check], [capture], [castle],
    [promoted], [checkmate], and [en_passant] represent their counterparts in
    [Move_record.gen_record], but default to their "normal" values for
    concision. *)
let test_record ?(is_check = false) ?(capture = false) ?(castle = false)
    ?promoted ?(checkmate = false) ?(en_passant = false) piece_type color
    (col1, row1) (col2, row2) =
  let start = Utils.Location.init_loc col1 row1 in
  let finish = Utils.Location.init_loc col2 row2 in
  Board.Move_record.gen_record piece_type color start finish is_check capture
    castle promoted checkmate en_passant

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
        (test_record Pawn White ('E', 2) ('E', 4))
        "e4";
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
