(* @author Aidan McNay (acm289) *)

open OUnit2

(** The type of outcome a test can have *)
type outcome =
  | Record of Board.Move_record.t
  | Invalid
  | Puts_in_check

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

type test =
  string * Piece.Types.color * Utils.Location.t * Utils.Location.t * outcome
(** The type of a test. A test [(name, color, start, finish, outcome)] indicates
    a test with name [name], indicating that [color] moving a piece from [start]
    to [finish] should have outcome [outcome]. *)

(**[sample_game_history] is the move history of the game played between
   Jan-Krzysztof Duda and Wesley So in 2019.05.17 during the Moscow Grand prix.
   Source: https://ratings.fide.com/view_pgn.phtml?code=217748*)
let sample_game_history =
  [
    "e4";
    "e5";
    "Nf3";
    "Nc6";
    "Bc4";
    "Bc5";
    "c3";
    "Nf6";
    "d3";
    "O-O";
    "O-O";
    "d5";
    "exd5";
    "Nxd5";
    "a4";
    "a6";
    "Re1";
    "Bg4";
    "Nbd2";
    "Kh8";
    "h3";
    "Bh5";
    "Ne4";
    "Ba7";
    "Ng3";
    "Bg6";
    "Nxe5";
    "Nxe5";
    "Rxe5";
    "Nb6";
    "Qf3";
    "c6";
    "Bf4";
    "Bb8";
    "Ree1";
    "Nxc4";
    "dxc4";
    "Qh4";
    "Ne2";
    "Ba7";
    "Bd6";
    "Rfe8";
    "Nf4";
    "Bc2";
    "c5";
    "a5";
    "Re2";
    "Bb3";
    "Ra3";
    "1-0";
  ]

(** [intermediate_board] is an intermediate board state, representing a game
    part-way done. It can be viewed at
    https://lichess.org/editor/2b5/4Bpbp/7r/p1Np4/2p2P1P/5P1p/1k1P4/1B3R1K_w_-_-_0_1?color=white *)
let intermediate_board =
  Piece.Pieces.
    [
      init Bishop White (Utils.Location.init_loc 'B' 1);
      init Rook White (Utils.Location.init_loc 'F' 1);
      init King White (Utils.Location.init_loc 'H' 1);
      init King Black (Utils.Location.init_loc 'B' 2);
      init Pawn White (Utils.Location.init_loc 'D' 2);
      init Pawn White (Utils.Location.init_loc 'F' 3);
      init Pawn Black (Utils.Location.init_loc 'H' 3);
      init Pawn Black (Utils.Location.init_loc 'C' 4);
      init Pawn White (Utils.Location.init_loc 'F' 4);
      init Pawn White (Utils.Location.init_loc 'H' 4);
      init Pawn Black (Utils.Location.init_loc 'A' 5);
      init Knight White (Utils.Location.init_loc 'C' 5);
      init Pawn Black (Utils.Location.init_loc 'D' 5);
      init Rook Black (Utils.Location.init_loc 'H' 6);
      init Bishop White (Utils.Location.init_loc 'E' 7);
      init Pawn Black (Utils.Location.init_loc 'F' 7);
      init Bishop Black (Utils.Location.init_loc 'G' 7);
      init Pawn Black (Utils.Location.init_loc 'H' 7);
      init Bishop Black (Utils.Location.init_loc 'C' 8);
    ]

(** The type of module for providing inputs to test a board's move *)
module type BoardTest = sig
  val board_state : Piece.Pieces.t list
  (** [board_state] is the current state of pieces on the board. *)

  val history : Board.Move_record.t list
  (** [history] is list of records of previous moves made, if needed. *)

  val moves_to_test : test list
  (** [moves_to_test] is a list of moves to test. An entry
      [(name, color, start, finish, outcome)] represents a test of name [name],
      testing that the [color] player moving a piece from [start] to [finish]
      will generate [outcome]; either the indicated record or an exception. *)
end

(** [compare_rec_attr attr rec1 rec2] is whether [attr rec1] and [attr rec2] are
    the same. *)
let compare_rec_attr attr rec1 rec2 = attr rec1 = attr rec2

(** [compare_records rec1 rec2] is whether [rec1] and [rec2] represent the same
    move record. *)
let compare_records rec1 rec2 =
  let attrs_to_check =
    [
      compare_rec_attr Board.Move_record.get_piece_type;
      compare_rec_attr (fun r ->
          Utils.Location.str_of_loc (Board.Move_record.get_start r));
      compare_rec_attr (fun r ->
          Utils.Location.str_of_loc (Board.Move_record.get_finish r));
      compare_rec_attr Board.Move_record.was_check;
      compare_rec_attr Board.Move_record.was_capture;
      compare_rec_attr Board.Move_record.was_castle;
      compare_rec_attr Board.Move_record.was_promotion;
      compare_rec_attr Board.Move_record.get_en_passant;
      compare_rec_attr Board.Move_record.get_color;
      compare_rec_attr Board.Move_record.get_checkmate;
    ]
  in
  List.for_all (fun cmp -> cmp rec1 rec2) attrs_to_check

(** A functor for testing the given moves in a [BoardTest] module. *)
module BoardTester (Test : BoardTest) = struct
  (** [board_to_test] is the board we want to test, given by [Test]*)
  let board_to_test = Board.Chessboard.mk_board Test.board_state Test.history []

  (** [get_new_record color start finish] gets the record from the [color]
      player moving a piece from [start] to [finish] on the board under test. *)
  let get_new_record color start finish =
    let new_board =
      Board.Chessboard.move_piece board_to_test color start finish
    in
    Board.Chessboard.last_move new_board

  (** [test_move color start finish outcome] checks that [color] moving a piece
      from [start] to [finish] generates [outcome]. *)
  let test_move color start finish outcome =
    match outcome with
    | Record r ->
        assert_equal r
          (get_new_record color start finish)
          ~cmp:compare_records
          ~printer:(Board.Alg_notation.move_record_to_alg_notation [])
    | Invalid ->
        assert_raises Board.Chessboard.Invalid_move (fun () ->
            get_new_record color start finish)
    | Puts_in_check ->
        assert_raises Board.Chessboard.Puts_in_check (fun () ->
            get_new_record color start finish)

  (** [tests] are the tests to run in an OUnit Test Suite. *)
  let tests =
    let gen_test (name, color, start, finish, outcome) =
      name >:: fun _ -> test_move color start finish outcome
    in
    List.map gen_test Test.moves_to_test
end
