(* @author Aidan McNay (acm289) *)

(** [get_king color pieces] gets the [color] king from [pieces]. *)
let get_king color pieces =
  let is_king piece =
    Piece.Pieces.get_type piece = Piece.Types.King
    && Piece.Pieces.get_color piece = color
  in
  match List.filter is_king pieces with
  | [ k ] -> k
  | [] -> failwith "No king detected"
  | _ :: _ -> failwith "Multiple kings detected"

(** [can_take_king king pieces piece] is whether [piece] can take [king] on a
    board with [pieces]. *)
let can_take_king king pieces piece =
  if Piece.Pieces.get_color piece = Piece.Pieces.get_color king then false
  else
    let valid_moves = Piece.Pieces.get_valid_moves piece pieces in
    let piece_location = Piece.Pieces.get_loc piece in
    let king_location = Piece.Pieces.get_loc king in
    let moves_take_king moves =
      Utils.Location.apply_moves piece_location moves = king_location
    in
    List.exists moves_take_king valid_moves

let in_check color pieces =
  let king = get_king color pieces in
  List.exists (can_take_king king pieces) pieces

(** [can_fix_check color pieces piece] is whether [color] moving [piece] on
    [pieces] can result in a state where [color] isn't in check. *)
let can_fix_check color pieces piece =
  if Piece.Pieces.get_color piece <> color then false
  else
    let valid_moves = Piece.Pieces.get_valid_moves piece pieces in
    let curr_loc = Piece.Pieces.get_loc piece in
    let moves_fix_check moves =
      let new_loc = Utils.Location.apply_moves curr_loc moves in
      let new_pieces =
        List.map
          (fun piece ->
            if Piece.Pieces.get_loc piece = curr_loc then
              Piece.Pieces.set_loc piece new_loc
            else piece)
          pieces
      in
      Bool.not (in_check color new_pieces)
    in
    List.exists moves_fix_check valid_moves

let in_checkmate color pieces =
  if Bool.not (in_check color pieces) then false
  else Bool.not (List.exists (can_fix_check color pieces) pieces)
