(* @author Aidan McNay (acm289) *)

open OUnit2

(** The type of outcome a test can have *)
type outcome =
  | Record of Board.Move_record.t
  | Invalid
  | Puts_in_check

(** The type of module for providing inputs to test a board's move *)
module type BoardTest = sig
  val board_state : Piece.Pieces.t list
  (** [board_state] is the current state of pieces on the board. *)

  val history : Board.Move_record.t list
  (** [history] is list of records of previous moves made, if needed. *)

  val moves_to_test :
    (string * Piece.Types.color * Utils.Location.t * Utils.Location.t * outcome)
    list
  (** [moves_to_test] is a list of moves to test. An entry
      [(name, color, start, finish, outcome)] represents a test of name [name],
      testing that the [color] player moving a piece from [start] to [finish]
      will generate [outcome]; either the indicated record or an exception. *)
end

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
    | Record r -> assert_equal r (get_new_record color start finish)
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
