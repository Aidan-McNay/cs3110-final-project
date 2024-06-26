(* @author Aidan McNay (acm289) *)

(** [is_king color piece] is whether [piece] is a king of color [color]. *)
let is_king color piece_type piece_color =
  piece_type = Piece.Types.King && piece_color = color

let is_castle color piece finish =
  let start = Piece.Pieces.get_loc piece in
  let piece_type = Piece.Pieces.get_type piece in
  let piece_color = Piece.Pieces.get_color piece in
  if Bool.not (is_king color piece_type piece_color) then false
  else
    let king_rank =
      match color with
      | Piece.Types.White -> 1
      | Piece.Types.Black -> 8
    in
    start = Utils.Location.init_loc 'E' king_rank
    && (finish = Utils.Location.init_loc 'G' king_rank
       || finish = Utils.Location.init_loc 'C' king_rank)

(** [king_has_moved color records] is whether [records] indicates that [color]'s
    king has already moved. *)
let king_has_moved color records =
  let is_king_move record =
    let piece_type = Move_record.get_piece_type record in
    let piece_color = Move_record.get_color record in
    is_king color piece_type piece_color
  in
  List.exists is_king_move records

(** [in_between_loc start finish] is the space between [start] and [finish].
    Requires: [start] and [finish] correspond to a king's move when castling. *)
let in_between_loc start finish =
  let rank = Utils.Location.get_row start in
  match Utils.Location.get_col finish with
  | 'G' -> Utils.Location.init_loc 'F' rank
  | 'C' -> Utils.Location.init_loc 'D' rank
  | _ -> failwith "Violated Prerequisite" [@coverage off]

(** [can_be_taken color pieces loc] is whether a piece at [loc] could be taken
    by [color] on a board with [pieces]. *)
let can_be_taken color pieces loc =
  let could_take_at_loc piece =
    if Piece.Pieces.get_color piece <> color then false
    else
      let curr_loc = Piece.Pieces.get_loc piece in
      let valid_moves = Piece.Pieces.get_valid_moves piece pieces in
      List.exists
        (fun moves -> Utils.Location.apply_moves curr_loc moves = loc)
        valid_moves
  in
  List.exists could_take_at_loc pieces

(** [which_rook_loc finish] determines the location of the rook to castle with,
    given that the king will end castling at [finish]. Requires: [finish] be a
    valid ending spot for a king's castle. *)
let which_rook_loc finish =
  let rank = Utils.Location.get_row finish in
  match Utils.Location.get_col finish with
  | 'G' -> Utils.Location.init_loc 'H' rank
  | 'C' -> Utils.Location.init_loc 'A' rank
  | _ -> failwith "Violated Prerequisite" [@coverage off]

(** [rook_still_there color loc pieces records] is whether a rook of color
    [color] is at [loc] in [pieces], and hasn't moved according to [records]. *)
let rook_still_there color loc pieces records =
  let is_rook piece =
    Piece.Pieces.get_type piece = Piece.Types.Rook
    && Piece.Pieces.get_color piece = color
    && Piece.Pieces.get_loc piece = loc
  in
  let rook_at_loc = List.exists is_rook pieces in
  let is_rook_record piece_type piece_color piece_loc =
    piece_type = Piece.Types.Rook && piece_color = color && piece_loc = loc
  in
  let is_rook_move record =
    let piece_type = Move_record.get_piece_type record in
    let piece_color = Move_record.get_color record in
    let piece_loc = Move_record.get_start record in
    is_rook_record piece_type piece_color piece_loc
  in
  let rook_has_not_moved = Bool.not (List.exists is_rook_move records) in
  rook_at_loc && rook_has_not_moved

(** [must_be_empty_locs start finish] are the locations that must be empty for a
    king to castle to [finish]. Requires: [finish] be a valid ending spot for a
    king's castle. *)
let must_be_empty_locs finish =
  let rank = Utils.Location.get_row finish in
  let cols =
    match Utils.Location.get_col finish with
    | 'G' -> [ 'F'; 'G' ]
    | 'C' -> [ 'B'; 'C'; 'D' ]
    | _ -> failwith "Violated Prerequisite" [@coverage off]
  in
  List.map (fun c -> Utils.Location.init_loc c rank) cols

(** [piece_exists pieces loc] is whether a piece exists at [loc] in [pieces]. *)
let piece_exists pieces loc =
  List.exists (fun piece -> Piece.Pieces.get_loc piece = loc) pieces

let can_castle color king finish pieces records =
  let start = Piece.Pieces.get_loc king in
  if king_has_moved color records then false
  else
    let locs_to_verify = [ start; in_between_loc start finish; finish ] in
    if
      List.exists
        (can_be_taken (Piece.Types.opposite color) pieces)
        locs_to_verify
    then false
    else
      let target_rook_loc = which_rook_loc finish in
      if Bool.not (rook_still_there color target_rook_loc pieces records) then
        false
      else
        let should_be_empty_locs = must_be_empty_locs finish in
        Bool.not (List.exists (piece_exists pieces) should_be_empty_locs)

(** [rook_finish_loc finish] is where a rook should end up after a castle,
    providing that the king castles to [finish]. Requires: [finish] is a valid
    location for a king to end up after a castle. *)
let rook_finish_loc finish =
  let rank = Utils.Location.get_row finish in
  match Utils.Location.get_col finish with
  | 'G' -> Utils.Location.init_loc 'F' rank
  | 'C' -> Utils.Location.init_loc 'D' rank
  | _ -> failwith "Violated Prerequisite" [@coverage off]

let castle color pieces piece finish _ =
  let move_piece start finish piece_list =
    List.map
      (fun piece ->
        if Piece.Pieces.get_loc piece = start then
          Piece.Pieces.set_loc piece finish
        else piece)
      piece_list
  in
  let king_piece = piece in
  let start = Piece.Pieces.get_loc king_piece in
  let rook_start = which_rook_loc finish in
  let rook_finish = rook_finish_loc finish in
  let new_pieces =
    pieces |> move_piece start finish |> move_piece rook_start rook_finish
  in
  let is_check = Check.in_check (Piece.Types.opposite color) new_pieces in
  let is_checkmate =
    Check.in_checkmate (Piece.Types.opposite color) new_pieces
  in
  let color = Piece.Pieces.get_color king_piece in
  let record =
    Move_record.gen_record Piece.Types.King color start finish is_check false
      true None is_checkmate false
  in
  (new_pieces, record)
