(* @author Aidan McNay (acm289) *)

(** [is_valid_move color start finish] is whether moving something from [start]
    to [finish] could represent a valid en passant move for [color]. *)
let is_valid_move color start finish =
  let curr_rank, possible_moves =
    match color with
    | Piece.Types.White -> (5, Utils.Move.[ [ Up; Left ]; [ Up; Right ] ])
    | Piece.Types.Black -> (4, Utils.Move.[ [ Down; Left ]; [ Down; Right ] ])
  in
  if curr_rank <> Utils.Location.get_row start then false
  else
    let moves_start_to_finish moves =
      try Utils.Location.apply_moves start moves = finish
      with Utils.Location.Off_board -> false
    in
    List.exists moves_start_to_finish possible_moves

let is_en_passant color piece finish pieces =
  if Piece.Pieces.get_type piece <> Piece.Types.Pawn then false
  else
    let start = Piece.Pieces.get_loc piece in
    if Bool.not (is_valid_move color start finish) then false
    else
      let normal_capture_piece = Piece.Pieces.piece_at_loc pieces finish in
      match normal_capture_piece with
      | Some _ -> false
      | None -> true

(** [pawn_moved_two record] is whether [record] represents a movement of a pawn
    by two spaces. *)
let pawn_moved_two record =
  let piece_type = Move_record.get_piece_type record in
  if piece_type <> Piece.Types.Pawn then false
  else
    let start_row, end_row =
      match Move_record.get_color record with
      | Piece.Types.White -> (2, 4)
      | Piece.Types.Black -> (7, 5)
    in
    let rows_to_compare =
      [
        (Move_record.get_start record, start_row);
        (Move_record.get_finish record, end_row);
      ]
    in
    List.for_all
      (fun (pos, row) -> Utils.Location.get_row pos = row)
      rows_to_compare

(** [is_valid_capture color target start finish] is whether [color] moving a
    pawn from [start] to [finish] would capture another pawn on [target],
    according to the rules of en passant. *)
let is_valid_capture target start finish =
  Utils.Location.(
    get_row target = get_row start && get_col target = get_col finish)

let can_en_passant color piece finish records =
  match records with
  | [] -> false
  | last_move :: _ ->
      if Move_record.get_color last_move = color then false
      else if Bool.not (pawn_moved_two last_move) then false
      else
        let start = Piece.Pieces.get_loc piece in
        is_valid_capture (Move_record.get_finish last_move) start finish

exception Cant_en_passant

let en_passant color pieces pawn_to_move finish _ =
  let start = Piece.Pieces.get_loc pawn_to_move in
  let target_col = Utils.Location.get_col finish in
  let target_row = Utils.Location.get_row start in
  let target_loc = Utils.Location.init_loc target_col target_row in
  let pawn_to_remove =
    match
      List.filter (fun piece -> Piece.Pieces.get_loc piece = target_loc) pieces
    with
    | [ p ] -> p
    | _ -> failwith "Violated Prerequisites" [@coverage off]
  in
  let pieces_without_target =
    List.filter (fun piece -> Piece.Pieces.get_loc piece <> target_loc) pieces
  in
  let new_pieces =
    List.map
      (fun piece ->
        if Piece.Pieces.get_loc piece = start then
          Piece.Pieces.set_loc piece finish
        else piece)
      pieces_without_target
  in
  let is_check = Check.in_check (Piece.Types.opposite color) new_pieces in
  let is_checkmate =
    Check.in_checkmate (Piece.Types.opposite color) new_pieces
  in
  let color = Piece.Pieces.get_color pawn_to_move in
  let record =
    Move_record.gen_record Piece.Types.Pawn color start finish is_check true
      false None is_checkmate true
  in
  (new_pieces, record, pawn_to_remove)
