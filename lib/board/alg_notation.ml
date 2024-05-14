(* @author Andro Janashia (aj454), Aidan McNay (acm289) *)

let checkmate_entry = function
  | Piece.Types.White -> "1-0"
  | Piece.Types.Black -> "0-1"

(** [name_of_piece_type piece_type] is the string representing [piece_type] in
    algebraic notation. *)
let name_of_piece_type piece_type =
  match piece_type with
  | Piece.Types.Pawn -> ""
  | Piece.Types.Knight -> "N"
  | Piece.Types.Bishop -> "B"
  | Piece.Types.Rook -> "R"
  | Piece.Types.Queen -> "Q"
  | Piece.Types.King -> "K"

(** [castle_str record] is the algebraic notation for the castle represented by
    [record]. Requires: [record] represents a castle move. *)
let castle_str record =
  let finish = Move_record.get_finish record in
  let kingside = Utils.Location.get_col finish = 'G' in
  if kingside then "O-O" else "O-O-O"

(** [ambig_str ambig_pieces record] is the string needed to disambiguate the
    move in [record] from the other possible locations in [ambig_pieces]. *)
let ambig_str ambig_pieces record =
  let start = Move_record.get_start record in
  let file_str = String.make 1 (Utils.Location.get_col_lowercase start) in
  let rank_str = string_of_int (Utils.Location.get_row start) in
  let file_rank_str = file_str ^ rank_str in
  let same_file piece =
    Utils.Location.get_col (Piece.Pieces.get_loc piece)
    = Utils.Location.get_col start
  in
  let same_rank piece =
    Utils.Location.get_row (Piece.Pieces.get_loc piece)
    = Utils.Location.get_row start
  in
  if List.is_empty ambig_pieces then ""
  else
    let removed_shared_file = List.filter same_file ambig_pieces in
    if List.is_empty removed_shared_file then file_str
    else
      let removed_shared_rank = List.filter same_rank ambig_pieces in
      if List.is_empty removed_shared_rank then rank_str else file_rank_str

(** [move_str ambig_pieces record] is the base move string for [record] (without
    indicating other attributes like promotion, check, or en passant), knowing
    that pieces in [ambig_pieces] could've also moved to the same location. *)
let move_str ambig_pieces record =
  if Move_record.was_castle record then castle_str record
  else
    let piece_type = Move_record.get_piece_type record in
    let start = Move_record.get_start record in
    let finish = Move_record.get_finish record in
    let capture = Move_record.was_capture record in
    let start_str = name_of_piece_type piece_type in
    let disambiguate_str = ambig_str ambig_pieces record in
    let pawn_capture_file =
      if
        capture && piece_type = Piece.Types.Pawn && List.length ambig_pieces < 1
      then String.make 1 (Utils.Location.get_col_lowercase start)
      else ""
    in
    let capture_str = if capture then "x" else "" in
    let finish_str = Utils.Location.str_of_loc_lowercase finish in
    start_str ^ disambiguate_str ^ pawn_capture_file ^ capture_str ^ finish_str

(** [promote_str record] is the string needed to represent the promotion in
    [record], if any. *)
let promote_str record =
  match Move_record.was_promotion record with
  | None -> ""
  | Some piece_type -> "=" ^ name_of_piece_type piece_type

(** [check_str record] is the string needed to represent the check or checkmate
    in [record], if any. *)
let check_str record =
  let check = Move_record.was_check record in
  let checkmate = Move_record.get_checkmate record in
  if checkmate then "#" else if check then "+" else ""

(** [en_passant_suffix record] is the suffix used to indicate en passant in
    [record], if needed. *)
let en_passant_suffix record =
  if Move_record.get_en_passant record then " e.p." else ""

let move_record_to_alg_notation ambig record =
  let str_funcs =
    [ move_str ambig; promote_str; check_str; en_passant_suffix ]
  in
  String.concat "" (List.map (fun f -> f record) str_funcs)

(** [is_end_of_game entry] checks whether [entry] indicates the end of a game. *)
let is_end_of_game = function
  | "1-0" -> true
  | "0-1" -> true
  | _ -> false

let rec move_history_to_algformat moves acc =
  match moves with
  | [] -> ""
  | [ hd ] ->
      if is_end_of_game hd then hd else string_of_int acc ^ ". " ^ hd ^ " "
  | [ fst; snd ] -> string_of_int acc ^ ". " ^ fst ^ " " ^ snd
  | h :: t :: r ->
      string_of_int acc ^ ". " ^ h ^ " " ^ t ^ " "
      ^ move_history_to_algformat r (acc + 1)

let print_move_history out_channel moves =
  Printf.fprintf out_channel "%s\n" (move_history_to_algformat moves 1)

let move_history_file filename moves =
  let oc = open_out filename in
  print_move_history oc moves;
  close_out oc
