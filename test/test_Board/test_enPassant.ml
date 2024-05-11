(* @author Aidan McNay (acm289) *)

open OUnit2

(** [en_passant_board_state] is a board state where en passant could potentially
    occur, with the white pawn at [white_start], and the black pawn at
    [black_start]. *)
let en_passant_board_state white_start black_start =
  Piece.Types.
    [
      Piece.Pieces.init Pawn White white_start;
      Piece.Pieces.init Pawn Black black_start;
      Piece.Pieces.init King White (Utils.Location.init_loc 'E' 1);
      Piece.Pieces.init King Black (Utils.Location.init_loc 'E' 8);
    ]

(** [en_passant_history start finish] is a list with a move record where a pawn
    moved from [start] to [finish]. *)
let en_passant_history start finish color =
  Piece.Types.
    [
      Board.Move_record.gen_record Pawn color start finish false false false
        None false false;
    ]

(** [valid_en_passant_move start finish] is a test for a valid en passant move
    from [start] to [finish] *)
let valid_en_passant_move name color start finish =
  let expected_record =
    Piece.Types.(
      Board.Move_record.gen_record Pawn color start finish false true false None
        false true)
  in
  (name, color, start, finish, Test_utils.Record expected_record)

(** [ValidWhiteEnPassantTest] is a module for a valid en passant move by white *)
module ValidWhiteEnPassantTest : Test_utils.BoardTest = struct
  (** [valid_start] is the valid starting position for the capturing pawn in en
      passant. *)
  let valid_start = Utils.Location.init_loc 'E' 5

  (** [valid_target_start] is a valid starting position for the captured pawn in
      en passant. *)
  let valid_target_start = Utils.Location.init_loc 'D' 5

  (** [valid_finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let valid_finish = Utils.Location.init_loc 'D' 6

  let board_state = en_passant_board_state valid_start valid_target_start

  (** [target_init] is where the target pawn started last turn. *)
  let target_init = Utils.Location.init_loc 'D' 7

  let history =
    en_passant_history target_init valid_target_start Piece.Types.Black

  let moves_to_test =
    [
      valid_en_passant_move "valid-white-en-passant" Piece.Types.White
        valid_start valid_finish;
    ]
end

(** [ValidBlackEnPassantTest] is a module for a valid en passant move by black *)
module ValidBlackEnPassantTest : Test_utils.BoardTest = struct
  (** [valid_start] is the valid starting position for the capturing pawn in en
      passant. *)
  let valid_start = Utils.Location.init_loc 'E' 4

  (** [valid_target_start] is a valid starting position for the captured pawn in
      en passant. *)
  let valid_target_start = Utils.Location.init_loc 'D' 4

  (** [valid_finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let valid_finish = Utils.Location.init_loc 'D' 3

  let board_state = en_passant_board_state valid_target_start valid_start

  (** [target_init] is where the target pawn started last turn. *)
  let target_init = Utils.Location.init_loc 'D' 2

  let history =
    en_passant_history target_init valid_target_start Piece.Types.White

  let moves_to_test =
    [
      valid_en_passant_move "valid-black-en-passant" Piece.Types.Black
        valid_start valid_finish;
    ]
end

(** [NotRecentMoveTest] is a module for testing an en passant move where the
    target pawn didn't move there most recently *)
module NotRecentMoveTest : Test_utils.BoardTest = struct
  (** [start] is the starting position for the capturing pawn in en passant. *)
  let start = Utils.Location.init_loc 'E' 5

  (** [target_start] is a starting position for the captured pawn in en passant. *)
  let target_start = Utils.Location.init_loc 'D' 5

  (** [finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let finish = Utils.Location.init_loc 'D' 6

  let board_state = en_passant_board_state start target_start
  let history = []

  let moves_to_test =
    [
      ("not-recent-move", Piece.Types.White, start, finish, Test_utils.Invalid);
    ]
end

(** [BoardEdgeTest] is a module for testing an en passant move where the
    capturing pawn is on the edge of the board *)
module BoardEdgeTest : Test_utils.BoardTest = struct
  (** [capture_start] is the starting position of the capturing pawn. *)
  let capture_start = Utils.Location.init_loc 'A' 5

  (** [capture_finish] is the ending position of the capturing pawn. *)
  let capture_finish = Utils.Location.init_loc 'B' 6

  (** [target_start] is the current position of the target pawn. *)
  let target_start = Utils.Location.init_loc 'B' 5

  (** [target_init] is the initial (starting) location of the target pawn. *)
  let target_init = Utils.Location.init_loc 'B' 7

  let board_state = en_passant_board_state capture_start target_start
  let history = en_passant_history target_init target_start Piece.Types.Black

  let moves_to_test =
    [
      valid_en_passant_move "board-edge" Piece.Types.White capture_start
        capture_finish;
    ]
end

(** [NormalCaptureTest] is a module for testing an en passant move where there's
    already a piece in the "normal" capture position (used largely for
    coverage). *)
module NormalCaptureTest : Test_utils.BoardTest = struct
  (** [start] is the starting position for the capturing pawn in en passant. *)
  let start = Utils.Location.init_loc 'E' 5

  (** [target_start] is a starting position for the captured pawn in en passant. *)
  let target_start = Utils.Location.init_loc 'D' 5

  (** [finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let finish = Utils.Location.init_loc 'D' 6

  let board_state =
    Piece.Types.(Piece.Pieces.init Bishop White finish)
    :: en_passant_board_state start target_start

  let history = []

  let moves_to_test =
    [ ("normal-capture", Piece.Types.White, start, finish, Test_utils.Invalid) ]
end

(** [NoPieceTest] is a module for testing an en passant when there's no piece to
    capture. *)
module NoPieceTest : Test_utils.BoardTest = struct
  (** [start] is the starting position for the capturing pawn in en passant. *)
  let start = Utils.Location.init_loc 'E' 5

  (** [target_start] is a starting position for the other pawn in en passant. *)
  let target_start = Utils.Location.init_loc 'C' 5

  (** [finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let finish = Utils.Location.init_loc 'D' 6

  (** [target_init] is the initial (starting) location of the other pawn. *)
  let target_init = Utils.Location.init_loc 'C' 7

  let board_state = en_passant_board_state start target_start
  let history = en_passant_history target_init target_start Piece.Types.Black

  let moves_to_test =
    [ ("no-piece", Piece.Types.White, start, finish, Test_utils.Invalid) ]
end

(** [NotAPawnTest] is a module for testing an en passant move where the piece
    that moved last turn wasn't a pawn *)
module NotAPawnTest : Test_utils.BoardTest = struct
  (** [start] is the starting position for the capturing pawn in en passant. *)
  let start = Utils.Location.init_loc 'E' 5

  (** [target_start] is a starting position for the captured pawn in en passant. *)
  let target_start = Utils.Location.init_loc 'D' 5

  (** [finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let finish = Utils.Location.init_loc 'D' 6

  let board_state =
    Piece.Types.
      [
        Piece.Pieces.init Pawn White start;
        Piece.Pieces.init Rook Black target_start;
        Piece.Pieces.init King White (Utils.Location.init_loc 'E' 1);
        Piece.Pieces.init King Black (Utils.Location.init_loc 'E' 8);
      ]

  (** [target_init] is where the "target" rook started last turn. *)
  let target_init = Utils.Location.init_loc 'D' 7

  let history =
    Piece.Types.
      [
        Board.Move_record.gen_record Rook Black target_init target_start false
          false false None false false;
      ]

  let moves_to_test =
    [ ("not-a-pawn", Piece.Types.White, start, finish, Test_utils.Invalid) ]
end

(** [WrongTargetColorTest] is a module for testing an en passant where we try to
    capture our own color *)
module WrongTargetColorTest : Test_utils.BoardTest = struct
  (** [start] is the starting position for the capturing pawn in en passant. *)
  let start = Utils.Location.init_loc 'E' 5

  (** [target_start] is a starting position for the captured pawn in en passant. *)
  let target_start = Utils.Location.init_loc 'D' 5

  (** [finish] is the valid ending position for the capturing pawn in en
      passant. *)
  let finish = Utils.Location.init_loc 'D' 6

  let board_state =
    Piece.Types.
      [
        Piece.Pieces.init Pawn White start;
        Piece.Pieces.init Pawn White target_start;
        Piece.Pieces.init King White (Utils.Location.init_loc 'E' 1);
        Piece.Pieces.init King Black (Utils.Location.init_loc 'E' 8);
      ]

  (** [target_init] is where the target pawn started last turn. *)
  let target_init = Utils.Location.init_loc 'D' 7

  let history = en_passant_history target_init target_start Piece.Types.White

  let moves_to_test =
    [
      ( "wrong-target-color",
        Piece.Types.White,
        start,
        finish,
        Test_utils.Invalid );
    ]
end

let test_modules : (module Test_utils.BoardTest) list =
  [
    (module ValidWhiteEnPassantTest);
    (module ValidBlackEnPassantTest);
    (module NotRecentMoveTest);
    (module BoardEdgeTest);
    (module NormalCaptureTest);
    (module NotAPawnTest);
    (module NoPieceTest);
    (module WrongTargetColorTest);
  ]

let tests =
  let get_tests m =
    let module S = (val m : Test_utils.BoardTest) in
    let module T = Test_utils.BoardTester (S) in
    T.tests
  in
  List.flatten (List.map get_tests test_modules)

let test_suite = "En Passant Test Suite" >::: tests
let _ = run_test_tt_main test_suite
