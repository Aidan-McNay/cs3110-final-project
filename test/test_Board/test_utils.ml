(* @author Aidan McNay (acm289) *)

open OUnit2

(** The type of outcome a test can have *)
type outcome =
  | Record of Board.Move_record.t
  | Invalid
  | Puts_in_check

type test =
  string * Piece.Types.color * Utils.Location.t * Utils.Location.t * outcome
(** The type of a test. A test [(name, color, start, finish, outcome)] indicates
    a test with name [name], indicating that [color] moving a piece from [start]
    to [finish] should have outcome [outcome]. *)

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
      compare_rec_attr Board.Move_record.get_alg_not;
      compare_rec_attr Board.Move_record.get_color;
      compare_rec_attr Board.Move_record.get_checkmate;
    ]
  in
  List.for_all (fun cmp -> cmp rec1 rec2) attrs_to_check

(** A functor for testing the given moves in a [BoardTest] module. *)
module BoardTester (Test : BoardTest) = struct
  (** [board_to_test] is the board we want to test, given by [Test]*)
  let board_to_test = Board.Chessboard.mk_board Test.board_state Test.history

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
          ~cmp:compare_records ~printer:Board.Move_record.get_alg_not
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
