(* @author Andro Janashia (aj454), Aidan McNay (acm289) *)

(** [castling_to_notation finish] checks whether kingside castling happened or
    queenside and returns the English Standard Algebraic Notation associated
    with what happened.*)
let castling_to_notation finish =
  let kingside = Utils.Location.get_col finish = 'G' in
  if kingside then "0-0" else "0-0-0"

(** [get_ambig_str ambig_pieces start] is the string needed to disambiguate
    [start] from the other possible locations in [ambig_pieces]. *)
let get_ambig_str ambig_pieces start =
  let file_str = String.make 1 (Utils.Location.get_col_lowercase start) in
  let rank_str = string_of_int (Utils.Location.get_row start) in
  let file_rank_str = file_str ^ rank_str in
  let different_file piece =
    Utils.Location.get_col (Piece.Pieces.get_loc piece)
    <> Utils.Location.get_col start
  in
  let different_rank piece =
    Utils.Location.get_row (Piece.Pieces.get_loc piece)
    <> Utils.Location.get_row start
  in
  if List.is_empty ambig_pieces then ""
  else
    let removed_shared_file = List.filter different_file ambig_pieces in
    if List.is_empty removed_shared_file then file_str
    else
      let removed_shared_rank = List.filter different_rank ambig_pieces in
      if List.is_empty removed_shared_rank then rank_str else file_rank_str

let move_record_to_alg_notation ambig record =
  let piece = Move_record.get_piece_type record in
  let start = Move_record.get_start record in
  let finish = Move_record.get_finish record in
  let check = Move_record.was_check record in
  let capture = Move_record.was_capture record in
  let castled = Move_record.was_castle record in
  let promotion = Move_record.was_promotion record in
  let checkmate = Move_record.get_checkmate record in
  let en_passant = Move_record.get_en_passant record in

  if castled then castling_to_notation finish
  else
    let promotion =
      match promotion with
      | Some prom -> Piece.Pieces.to_algebraic_notation prom
      | None -> ""
    in
    Piece.Pieces.to_algebraic_notation piece
    ^ get_ambig_str ambig start
    ^ (if capture && List.length ambig < 1 && piece = Piece.Types.Pawn then
         String.(lowercase_ascii (make 1 (Utils.Location.get_col start))) ^ "x"
       else "")
    ^ (if capture && piece <> Piece.Types.Pawn then "x" else "")
    ^ ""
    ^ String.lowercase_ascii (Utils.Location.str_of_loc finish)
    ^ promotion
    ^ (if checkmate then "#" else if check then "+" else "")
    ^ if en_passant then " e.p." else ""

let move_record_to_longalgnotation move_rec =
  let piece_type = Move_record.get_piece_type move_rec in
  let finish = Move_record.get_finish move_rec in
  let start = Move_record.get_start move_rec in
  let capture = Move_record.was_capture move_rec in
  let check = Move_record.was_check move_rec in
  let castle = Move_record.was_castle move_rec in
  let promotion =
    match Move_record.was_promotion move_rec with
    | Some prom -> Piece.Pieces.to_algebraic_notation prom
    | None -> ""
  in
  if castle then castling_to_notation finish
  else
    Piece.Pieces.to_algebraic_notation piece_type
    ^ String.lowercase_ascii (Utils.Location.str_of_loc start)
    ^ (if capture then "x" else "-")
    ^ String.lowercase_ascii (Utils.Location.str_of_loc finish)
    ^ promotion
    ^ if check then "+" else ""

let rec move_history_to_algformat moves acc =
  match moves with
  | [] -> ""
  | [ hd ] -> string_of_int acc ^ "," ^ hd ^ ","
  | [ fst; snd ] -> string_of_int acc ^ "," ^ fst ^ "," ^ snd
  | h :: t :: r ->
      string_of_int acc ^ "," ^ h ^ "," ^ t ^ "\n"
      ^ move_history_to_algformat r (acc + 1)

let print_move_history out_channel moves =
  Printf.fprintf out_channel "%s\n" "Turn,White,Black";
  Printf.fprintf out_channel "%s\n" (move_history_to_algformat moves 1)

let move_history_file filename moves =
  let oc = open_out filename in
  print_move_history oc moves;
  close_out oc
