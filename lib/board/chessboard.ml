(* @author Aidan McNay (acm289) *)

type t = {
  pieces_on_board : Piece.Pieces.t list;
  captured_by_white : Piece.Pieces.t list;
  captured_by_black : Piece.Pieces.t list;
  moves : Move_record.t list;
  alg_notation : string list;
}
(* AF: The record [{pieces_on_board = p; captured_by_white = w;
   captured_by_black = b; moves = m}] represents a chess board with the pieces
   in [p] on the board, with white having caputed the pieces in [w] and black
   having captured the pieces in [b]. [m] is a list of [move_record]s that have
   occured so far in the game. *)
(* RI: The length of [alg_notation] and [moves] must be the same if the game
   isn't finished. [captured_by_white] must only contain pieces of color
   [Piece.Types.Black]. [captured_by_black] must only contain pieces of color
   [Piece.Types.White]. [pieces_on_board] must represent a valid chess game;
   notably, it must contain exactly one piece of type [Piece.Types.King] for
   each color. *)

let initial =
  {
    pieces_on_board = Piece.Pieces.start_state;
    captured_by_white = [];
    captured_by_black = [];
    moves = [];
    alg_notation = [];
  }

let mk_board pieces records alg_nots =
  {
    pieces_on_board = pieces;
    captured_by_white = [];
    captured_by_black = [];
    moves = records;
    alg_notation = alg_nots;
  }

exception Invalid_move
exception Puts_in_check

(** [move_piece_list piece_list start finish] is [piece_list] with the piece at
    [start] moved to [finish]. *)
let move_piece_list piece_list start finish =
  List.map
    (fun piece ->
      if Piece.Pieces.get_loc piece = start then
        Piece.Pieces.set_loc piece finish
      else piece)
    piece_list

(** [capture_piece board loc] is [board] with the piece at [loc] captured, as
    well as whether a piece was captured. Evaluates to [board, false] if there
    is no piece at [loc]. *)
let capture_piece board loc =
  match Piece.Pieces.piece_at_loc board.pieces_on_board loc with
  | Some piece ->
      let captured_by_white, captured_by_black =
        match Piece.Pieces.get_color piece with
        | Piece.Types.Black ->
            (piece :: board.captured_by_white, board.captured_by_black)
        | Piece.Types.White ->
            (board.captured_by_white, piece :: board.captured_by_black)
      in
      let new_pieces_on_board =
        List.filter
          (fun piece -> Piece.Pieces.get_loc piece <> loc)
          board.pieces_on_board
      in
      ( {
          pieces_on_board = new_pieces_on_board;
          captured_by_white;
          captured_by_black;
          moves = board.moves;
          alg_notation = board.alg_notation;
        },
        true )
  | None -> (board, false)

(** [is_valid_move board piece new_loc] is whether moving [piece] to [new_loc]
    on [board] is a valid move. *)
let is_valid_move board piece new_loc =
  let curr_loc = Piece.Pieces.get_loc piece in
  let valid_moves = Piece.Pieces.get_valid_moves piece board.pieces_on_board in
  List.exists
    (fun moves -> Utils.Location.apply_moves curr_loc moves = new_loc)
    valid_moves

(** [do_castle board color start finish] is [board] after [color] castles from
    [start] to [finish]. Requires: [color] moving a piece from [start] to
    [finish] represents a castle. *)
let do_castle
    {
      pieces_on_board;
      captured_by_white;
      captured_by_black;
      moves;
      alg_notation;
    } color piece finish =
  if Castle.can_castle color piece finish pieces_on_board moves then
    let new_pieces, new_record =
      Castle.castle color pieces_on_board piece finish moves
    in
    {
      pieces_on_board = new_pieces;
      captured_by_white;
      captured_by_black;
      moves = new_record :: moves;
      alg_notation =
        Alg_notation.move_record_to_alg_notation [] new_record :: alg_notation;
    }
  else raise Invalid_move

(** [do_en_passant board color start finish] is [board] after [color] does an
    en_passant from [start] to [finish]. Requires: [color] moving a piece from
    [start] to [finish] represents a castle. *)
let do_en_passant
    {
      pieces_on_board;
      captured_by_white;
      captured_by_black;
      moves;
      alg_notation;
    } color piece finish =
  if En_passant.can_en_passant color piece finish moves then
    let new_pieces, new_record, captured_piece =
      En_passant.en_passant color pieces_on_board piece finish moves
    in
    let captured_by_white, captured_by_black =
      match color with
      | Piece.Types.White ->
          (captured_piece :: captured_by_white, captured_by_black)
      | Piece.Types.Black ->
          (captured_by_white, captured_piece :: captured_by_black)
    in
    {
      pieces_on_board = new_pieces;
      captured_by_white;
      captured_by_black;
      moves = new_record :: moves;
      alg_notation =
        Alg_notation.move_record_to_alg_notation [] new_record :: alg_notation;
    }
  else raise Invalid_move

(** [should_promote piece] is whether [piece] needs to be promoted. *)
let should_promote piece =
  if Piece.Pieces.get_type piece <> Piece.Types.Pawn then false
  else
    let piece_color = Piece.Pieces.get_color piece in
    let promotion_rank =
      match piece_color with
      | Piece.Types.White -> 8
      | Piece.Types.Black -> 1
    in
    if Utils.Location.get_row (Piece.Pieces.get_loc piece) <> promotion_rank
    then false
    else true

(** [promoted_piece piece] is [piece] after it's been promoted, prompting the
    user for the new type. Evaluates to [piece] if [piece] shouldn't be
    promoted. *)
let promoted_piece piece =
  let piece_color = Piece.Pieces.get_color piece in
  if should_promote piece then
    Piece.Pieces.set_type piece (Prompt.prompt_promotion piece_color)
  else piece

(** [promotion_check pieces] is [pieces] after promoting any pawn that needs to
    be promoted, prompting the user if necessary, as well as whether a pawn was
    promoted. *)
let promotion_check pieces =
  (List.map promoted_piece pieces, List.exists should_promote pieces)

(** [promoted_to pieces promoted finish] is the piece that was promoted to at
    [finish] on [pieces]. If [promoted] is [false], indicating no promotion,
    then [promoted_to pieces promoted finish] is [None]. Requires: if [promoted]
    is [true], then there is a piece at [finish] in [pieces]. *)
let promoted_to pieces promoted finish =
  if promoted then
    Some
      (match Piece.Pieces.(piece_at_loc pieces finish) with
      | Some piece -> Piece.Pieces.get_type piece
      | None -> failwith "Violated Prerequisites" [@coverage off])
  else None

(** [ambig_pieces board piece finish] returns a list of pieces in [board] that
    are of the same piece type as [piece] and that could have also been moved to
    [finish]. *)
let ambig_pieces board piece finish =
  List.filter
    (fun pc ->
      Piece.Pieces.get_type pc = Piece.Pieces.get_type piece
      && Piece.Pieces.get_color pc = Piece.Pieces.get_color piece
      && is_valid_move board pc finish
      && Piece.Pieces.get_loc piece <> Piece.Pieces.get_loc pc)
    board.pieces_on_board

let move_piece board color start finish =
  match Piece.Pieces.piece_at_loc board.pieces_on_board start with
  | None -> raise Invalid_move
  | Some piece ->
      if color <> Piece.Pieces.get_color piece then raise Invalid_move
      else if Castle.is_castle color piece finish then
        do_castle board color piece finish
      else if En_passant.is_en_passant color piece finish board.pieces_on_board
      then do_en_passant board color piece finish
      else if Bool.not (is_valid_move board piece finish) then
        raise Invalid_move
      else
        let new_board, captured_a_piece = capture_piece board finish in
        let pieces_on_board =
          move_piece_list new_board.pieces_on_board start finish
        in
        let pieces_on_board, promoted = promotion_check pieces_on_board in
        if Check.in_check color pieces_on_board then raise Puts_in_check
        else
          let check_opp =
            Check.in_check (Piece.Types.opposite color) pieces_on_board
          in
          let checkmate_opp =
            Check.in_checkmate (Piece.Types.opposite color) pieces_on_board
          in
          let promoted_type = promoted_to pieces_on_board promoted finish in
          let ambig = ambig_pieces board piece finish in
          let piece_type = Piece.Pieces.get_type piece in
          let color = Piece.Pieces.get_color piece in
          let new_move_record =
            Move_record.gen_record piece_type color start finish check_opp
              captured_a_piece false promoted_type checkmate_opp false
          in
          let new_alg_not =
            let alg_not =
              Alg_notation.move_record_to_alg_notation ambig new_move_record
            in
            if checkmate_opp then
              [ Alg_notation.checkmate_entry color; alg_not ]
            else [ alg_not ]
          in
          {
            pieces_on_board;
            captured_by_white = new_board.captured_by_white;
            captured_by_black = new_board.captured_by_black;
            moves = new_move_record :: new_board.moves;
            alg_notation = new_alg_not @ new_board.alg_notation;
          }

let in_checkmate color board = Check.in_checkmate color board.pieces_on_board

exception No_moves_made

let last_move board =
  match board.moves with
  | [] -> raise No_moves_made
  | h :: _ -> h

let move_history board = board.moves

let image_at_loc ?(selected = false) board loc bg =
  let bg =
    if selected then Bogue.Draw.opaque (Bogue.Draw.find_color "#FF00FF") else bg
  in
  match Piece.Pieces.piece_at_loc board.pieces_on_board loc with
  | Some piece -> Piece.Pieces.to_image piece bg
  | None ->
      Bogue.Widget.box ~w:50 ~h:50
        ~style:(Bogue.Style.of_bg (Bogue.Style.Solid bg))
        ()

let get_rev_alg_notation board = List.rev board.alg_notation
