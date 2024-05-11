(* @author Aidan McNay (acm289) *)

open OUnit2

(** [castle_board_state color king_loc rook_loc other_king_loc] is a board state
    where a castle could potentially occur, with the [color] king at [king_loc],
    and the [color] rook at [rook_loc], with the other king at [other_king_loc]. *)
let castle_board_state color king_loc rook_loc other_king_loc =
  Piece.Types.
    [
      Piece.Pieces.init King color king_loc;
      Piece.Pieces.init Rook color rook_loc;
      Piece.Pieces.init King (Piece.Types.opposite color) other_king_loc;
    ]

(** [valid_castle_move name color start finish] is a test named [name] for a
    valid castling for the [color] player, moving the king from [start] to
    [finish] *)
let valid_castle_move name color start finish =
  let expected_record =
    Piece.Types.(
      Board.Move_record.gen_record King color start finish false false true None
        false false)
  in
  (name, color, start, finish, Test_utils.Record expected_record)

(** [ValidWhiteCastleKingSideTest] is a module for a valid castling by white (on
    the king side) *)
module ValidWhiteCastleKingSideTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 1

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'G' 1

  (** [rook_start] is the starting location of the rook. *)
  let rook_start = Utils.Location.init_loc 'H' 1

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 8

  let board_state =
    castle_board_state Piece.Types.White king_start rook_start other_king_start

  let history = []

  let moves_to_test =
    [
      valid_castle_move "white-king-side" Piece.Types.White king_start
        king_finish;
    ]
end

(** [ValidBlackCastleQueenSideTest] is a module for a valid castling by black
    (on the queen side) *)
module ValidBlackCastleQueenSideTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 8

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'C' 8

  (** [rook_start] is the starting location of the rook. *)
  let rook_start = Utils.Location.init_loc 'A' 8

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 1

  let board_state =
    castle_board_state Piece.Types.Black king_start rook_start other_king_start

  let history = []

  let moves_to_test =
    [
      valid_castle_move "black-queen-side" Piece.Types.Black king_start
        king_finish;
    ]
end

(** [NoRookTest] is a module for testing a castle attempt without a rook *)
module NoRookTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 8

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'C' 8

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 1

  let board_state =
    Piece.Types.
      [
        Piece.Pieces.init King White king_start;
        Piece.Pieces.init King Black other_king_start;
      ]

  let history = []

  let moves_to_test =
    [
      ("no-rook", Piece.Types.Black, king_start, king_finish, Test_utils.Invalid);
    ]
end

(** [gen_history piece_type color curr_loc new_loc] is a move history of a
    [piece_type] piece of color [color] moving from [curr_loc] to [new_loc],
    then back. *)
let gen_history piece_type color curr_loc new_loc =
  let move_away_record =
    Board.Move_record.gen_record piece_type color curr_loc new_loc false false
      false None false false
  in
  let move_back_record =
    Board.Move_record.gen_record piece_type color new_loc curr_loc false false
      false None false false
  in
  [ move_away_record; move_back_record ]

(** [KingMovedTest] is a module for testing a castle attempt where the king has
    already moved *)
module KingMovedTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 1

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'G' 1

  (** [rook_start] is the starting location of the rook. *)
  let rook_start = Utils.Location.init_loc 'H' 1

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 8

  let board_state =
    castle_board_state Piece.Types.White king_start rook_start other_king_start

  (** [king_temp_loc] is the location where the king previously moved. *)
  let king_temp_loc = Utils.Location.init_loc 'E' 2

  let history =
    gen_history Piece.Types.King Piece.Types.White king_start king_temp_loc

  let moves_to_test =
    [
      ( "king-already-moved",
        Piece.Types.White,
        king_start,
        king_finish,
        Test_utils.Invalid );
    ]
end

(** [RookMovedTest] is a module for testing a castle attempt where the rook has
    already moved *)
module RookMovedTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 1

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'G' 1

  (** [rook_start] is the starting location of the rook. *)
  let rook_start = Utils.Location.init_loc 'H' 1

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 8

  let board_state =
    castle_board_state Piece.Types.White king_start rook_start other_king_start

  (** [rook_temp_loc] is the location where the rook previously moved. *)
  let rook_temp_loc = Utils.Location.init_loc 'H' 3

  let history =
    gen_history Piece.Types.Rook Piece.Types.White rook_start rook_temp_loc

  let moves_to_test =
    [
      ( "rook-already-moved",
        Piece.Types.White,
        king_start,
        king_finish,
        Test_utils.Invalid );
    ]
end

(** [InCheckTest] is a module for trying to castle when in check *)
module InCheckTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 1

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'G' 1

  (** [rook_start] is the starting location of the rook. *)
  let rook_start = Utils.Location.init_loc 'H' 1

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 8

  (** [checking_piece] is another piece on the board checking the king. *)
  let checking_piece =
    Piece.Types.(Piece.Pieces.init Bishop Black (Utils.Location.init_loc 'B' 4))

  let board_state =
    checking_piece
    :: castle_board_state Piece.Types.White king_start rook_start
         other_king_start

  let history = []

  let moves_to_test =
    [
      ( "king-in-check",
        Piece.Types.White,
        king_start,
        king_finish,
        Test_utils.Invalid );
    ]
end

(** [ThroughCheckTest] is a module for trying to castle when in check *)
module ThroughCheckTest : Test_utils.BoardTest = struct
  (** [king_start] is the starting location of the king. *)
  let king_start = Utils.Location.init_loc 'E' 1

  (** [king_finish] is the starting location of the king. *)
  let king_finish = Utils.Location.init_loc 'C' 1

  (** [rook_start] is the starting location of the rook. *)
  let rook_start = Utils.Location.init_loc 'A' 1

  (** [other_king_start] is the starting location of the other king. *)
  let other_king_start = Utils.Location.init_loc 'E' 8

  (** [checking_piece] is another piece on the board checking the king. *)
  let checking_piece =
    Piece.Types.(Piece.Pieces.init Bishop Black (Utils.Location.init_loc 'G' 4))

  let board_state =
    checking_piece
    :: castle_board_state Piece.Types.White king_start rook_start
         other_king_start

  let history = []

  let moves_to_test =
    [
      ( "king-through-check",
        Piece.Types.White,
        king_start,
        king_finish,
        Test_utils.Invalid );
    ]
end

let test_modules : (module Test_utils.BoardTest) list =
  [
    (module ValidWhiteCastleKingSideTest);
    (module ValidBlackCastleQueenSideTest);
    (module NoRookTest);
    (module KingMovedTest);
    (module RookMovedTest);
    (module InCheckTest);
    (module ThroughCheckTest);
  ]

let tests =
  let get_tests m =
    let module S = (val m : Test_utils.BoardTest) in
    let module T = Test_utils.BoardTester (S) in
    T.tests
  in
  List.flatten (List.map get_tests test_modules)

let test_suite = "Castle Test Suite" >::: tests
let _ = run_test_tt_main test_suite
