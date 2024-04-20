(* @author Aidan McNay (acm289) *)

type move_record = {
  piece : Piece.Pieces.t;
  start : Utils.Location.t;
  finish : Utils.Location.t;
  is_check : bool;
  captured_a_piece : bool;
  castled : bool;
  promoted: bool
}

type t = {
  pieces_on_board : Piece.Pieces.t list;
  captured_by_white : Piece.Pieces.t list;
  captured_by_black : Piece.Pieces.t list;
  moves : move_record list;
}

(* AF: The record [{pieces_on_board = p; captured_by_white = w;
   captured_by_black = b; moves = m}] represents a chess board with the pieces
   in [p] on the board, with white having caputed the pieces in [w] and black
   having captured the pieces in [b]. [m] is a list of [move_record]s that have
   occured so far in the game. *)
(* RI: The location must be a valid chess board location; [row] and [column]
   must be between 1 and 8, inclusive. *)

let start_pieces_list =
  Piece.Pieces.
    [
      (* White *)
      init Rook White (Utils.Location.init_loc 'A' 1);
      init Knight White (Utils.Location.init_loc 'B' 1);
      init Bishop White (Utils.Location.init_loc 'C' 1);
      init Queen White (Utils.Location.init_loc 'D' 1);
      init King White (Utils.Location.init_loc 'E' 1);
      init Bishop White (Utils.Location.init_loc 'F' 1);
      init Knight White (Utils.Location.init_loc 'G' 1);
      init Rook White (Utils.Location.init_loc 'H' 1);
      init Pawn White (Utils.Location.init_loc 'A' 2);
      init Pawn White (Utils.Location.init_loc 'B' 2);
      init Pawn White (Utils.Location.init_loc 'C' 2);
      init Pawn White (Utils.Location.init_loc 'D' 2);
      init Pawn White (Utils.Location.init_loc 'E' 2);
      init Pawn White (Utils.Location.init_loc 'F' 2);
      init Pawn White (Utils.Location.init_loc 'G' 2);
      init Pawn White (Utils.Location.init_loc 'H' 2);
      (* Black *)
      init Rook Black (Utils.Location.init_loc 'A' 8);
      init Knight Black (Utils.Location.init_loc 'B' 8);
      init Bishop Black (Utils.Location.init_loc 'C' 8);
      init Queen Black (Utils.Location.init_loc 'D' 8);
      init King Black (Utils.Location.init_loc 'E' 8);
      init Bishop Black (Utils.Location.init_loc 'F' 8);
      init Knight Black (Utils.Location.init_loc 'G' 8);
      init Rook Black (Utils.Location.init_loc 'H' 8);
      init Pawn Black (Utils.Location.init_loc 'A' 7);
      init Pawn Black (Utils.Location.init_loc 'B' 7);
      init Pawn Black (Utils.Location.init_loc 'C' 7);
      init Pawn Black (Utils.Location.init_loc 'D' 7);
      init Pawn Black (Utils.Location.init_loc 'E' 7);
      init Pawn Black (Utils.Location.init_loc 'F' 7);
      init Pawn Black (Utils.Location.init_loc 'G' 7);
      init Pawn Black (Utils.Location.init_loc 'H' 7);
    ]

let initial =
  {
    pieces_on_board = start_pieces_list;
    captured_by_white = [];
    captured_by_black = [];
    moves = [];
  }

(** [get_piece_points pieces] is the cumulative number of points that the pieces
    in [pieces] are worth. *)
let get_piece_points pieces =
  List.fold_left (fun acc piece -> acc + Piece.Pieces.get_points piece) 0 pieces

let get_points board color =
  match color with
  | Piece.Types.White -> get_piece_points board.captured_by_white
  | Piece.Types.Black -> get_piece_points board.captured_by_black

exception Invalid_move

(** [move_piece_list piece_list start finish] is [piece_list] with the piece at
    [start] moved to [finish]. *)
let move_piece_list piece_list start finish =
  List.map
    (fun piece ->
      if Piece.Pieces.get_loc piece = start then
        Piece.Pieces.set_loc piece finish
      else piece)
    piece_list

(** [piece_at_loc piece_list loc] is the piece in [piece_list] at [loc], if any. *)
let piece_at_loc piece_list loc =
  let piece_at_loc piece = Piece.Pieces.get_loc piece = loc in
  List.find_opt piece_at_loc piece_list

(** Added code*)
let get_pieces_on_board game = game.pieces_on_board


(** [capture_piece board loc] is [board] with the piece at [loc] captured, as
    well as whether a piece was captured. Evaluates to [board, false] if there
    is no piece at [loc]. *)
let capture_piece board loc =
  match piece_at_loc board.pieces_on_board loc with
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



let pieces_on_board_of_color board (color : Piece.Pieces.color) = 
  List.filter (fun piece -> Piece.Pieces.get_color piece = color) (board.pieces_on_board)


let check_board_for_promotion board (color : Piece.Pieces.color) =
  let row = if color = White then 8 else 1 in
  let color_pieces = pieces_on_board_of_color board color in
  let pawns = List.filter( fun piece -> Piece.Pieces.get_type piece = (Pawn : Piece.Types.piece_type)) color_pieces in
  let filtered = List.filter (fun pawn -> Utils.Location.get_row(Piece.Pieces.get_loc pawn) = row) pawns in
  if List.is_empty filtered then (Utils.Location.init_loc 'a' 1 ,false) else (Piece.Pieces.get_loc (List.nth filtered 0), true)
  

exception BigIssue_King_Went_Missing

let get_king board (color : Piece.Pieces.color) = 
  let updated_list = pieces_on_board_of_color board color in
  let king = List.filter (fun piece -> Piece.Pieces.get_type piece = (King : Piece.Types.piece_type)) updated_list in
  if List.length king <> 1 then raise BigIssue_King_Went_Missing else
    List.nth king 0

let check_all_color_move_to board (color1 : Piece.Pieces.color) (color2 : Piece.Pieces.color) = 
  let get_color1_board_pieces = pieces_on_board_of_color board color1 in
  let king_location = Piece.Pieces.get_loc (get_king board color2) in
  let all_pieces_can_capture_king = List.filter (fun piece -> is_valid_move board piece king_location) get_color1_board_pieces in
  if List.is_empty all_pieces_can_capture_king then true else false


let in_check board (color : Piece.Pieces.color) =
  match color with
  | White -> check_all_color_move_to board Black White
  | Black -> check_all_color_move_to board White Black


let move_piece board start finish =
  match piece_at_loc board.pieces_on_board start with
  | None -> raise Invalid_move
  | Some piece ->
      if Bool.not (is_valid_move board piece finish) then raise Invalid_move
      else
        let captured_board, captured_a_piece = capture_piece board finish in
        let pieces_on_board =
          move_piece_list captured_board.pieces_on_board start finish
        in
        let is_promoting = if Piece.Pieces.get_type piece = Pawn then 
          if Piece.Pieces.get_color piece = White && Utils.Location.get_row finish = 8 then true
          else if Utils.Location.get_row finish = 1 && Piece.Pieces.get_color piece = Black then true 
          else false
        else false in
        let new_move_record =
           if is_promoting then
          {piece; start; finish; is_check = in_check board (Piece.Pieces.get_color piece); captured_a_piece; castled=false; promoted= true}
           else
          {piece; start; finish; is_check = in_check board (Piece.Pieces.get_color piece); captured_a_piece; castled=false; promoted= false}
        in
        ( {
            pieces_on_board;
            captured_by_white = captured_board.captured_by_white;
            captured_by_black = captured_board.captured_by_black;
            moves = new_move_record :: captured_board.moves;
          },
          new_move_record )
   
let every_move board start_location color= 
  let all_moves = [Utils.Location.init_loc 'a' 1; Utils.Location.init_loc 'a' 2; Utils.Location.init_loc 'a' 3; Utils.Location.init_loc 'a' 4; Utils.Location.init_loc 'a' 5; 
  Utils.Location.init_loc 'a' 6; Utils.Location.init_loc 'a' 7; Utils.Location.init_loc 'b' 1; Utils.Location.init_loc 'b' 1; Utils.Location.init_loc 'b' 2; Utils.Location.init_loc 'b' 3; Utils.Location.init_loc 'b' 4; Utils.Location.init_loc 'b' 5; 
  Utils.Location.init_loc 'b' 6; Utils.Location.init_loc 'b' 7; Utils.Location.init_loc 'b' 8; Utils.Location.init_loc 'c' 1; Utils.Location.init_loc 'c' 2; Utils.Location.init_loc 'c' 3; Utils.Location.init_loc 'c' 4; Utils.Location.init_loc 'c' 5; 
  Utils.Location.init_loc 'c' 6; Utils.Location.init_loc 'c' 7; Utils.Location.init_loc 'c' 8; Utils.Location.init_loc 'd' 1; Utils.Location.init_loc 'd' 2; Utils.Location.init_loc 'd' 3; Utils.Location.init_loc 'd' 4; Utils.Location.init_loc 'd' 5; 
  Utils.Location.init_loc 'd' 6; Utils.Location.init_loc 'd' 7; Utils.Location.init_loc 'd' 8; Utils.Location.init_loc 'e' 1; Utils.Location.init_loc 'e' 2; Utils.Location.init_loc 'e' 3; Utils.Location.init_loc 'e' 4; Utils.Location.init_loc 'e' 5; 
  Utils.Location.init_loc 'e' 6; Utils.Location.init_loc 'e' 7; Utils.Location.init_loc 'e' 8; Utils.Location.init_loc 'f' 1; Utils.Location.init_loc 'f' 2; Utils.Location.init_loc 'f' 3; Utils.Location.init_loc 'f' 4; Utils.Location.init_loc 'f' 5; 
  Utils.Location.init_loc 'f' 6; Utils.Location.init_loc 'f' 7; Utils.Location.init_loc 'f' 8; Utils.Location.init_loc 'g' 1; Utils.Location.init_loc 'g' 2; Utils.Location.init_loc 'g' 3; Utils.Location.init_loc 'g' 4; Utils.Location.init_loc 'g' 5; 
  Utils.Location.init_loc 'g' 6; Utils.Location.init_loc 'g' 7; Utils.Location.init_loc 'g' 8; Utils.Location.init_loc 'h' 1; Utils.Location.init_loc 'h' 2; Utils.Location.init_loc 'h' 3; Utils.Location.init_loc 'h' 4; Utils.Location.init_loc 'h' 5; 
  Utils.Location.init_loc 'h' 6; Utils.Location.init_loc 'h' 7; Utils.Location.init_loc 'h' 8] in
  let all_allowed_moves = List.filter (fun end_location -> try let _ = fst(move_piece board start_location end_location) in true with | Invalid_move -> false) all_moves in
  let all_allowed_moves = List.map (fun end_location -> (fst(move_piece board start_location end_location), end_location)) all_allowed_moves in
  let all_moves_out_of_check = List.filter (fun board -> ((in_check (fst board) color))) all_allowed_moves in
  all_moves_out_of_check
  
let every_piece_check board deffense_color= 
  let deffense_piece = pieces_on_board_of_color board deffense_color in
  let answer = List.map (fun piece -> List.is_empty (every_move board (Piece.Pieces.get_loc piece) deffense_color)) deffense_piece in
  List.fold_left (fun acc answer -> acc && answer) true answer
let checkmate board (color : Piece.Pieces.color) = 
  match color with
  | White -> every_piece_check board (White : Piece.Pieces.color)
  | Black -> every_piece_check board (Black : Piece.Pieces.color)

let get_in_check_move_record record = record.is_check


exception No_moves_made


let last_move board =
  match board.moves with
  | [] -> raise No_moves_made
  | h :: _ -> h

let move_history board = board.moves

(** [loc_rep board loc] is the string representation of the location [loc] on
    [board]. *)
let loc_rep board loc =
  match
    List.find_opt
      (fun piece -> Piece.Pieces.get_loc piece = loc)
      board.pieces_on_board
  with
  | Some piece -> Piece.Pieces.to_string piece
  | None -> " "

(** [columns] is the list of columns on a chess board, in order of printing. *)
let columns = [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H' ]

(** [row_rep board row] is the string representation of row [row] in [board]. *)
let row_rep board row =
  let get_loc_rep = loc_rep board in
  let loc_reps =
    List.map (fun col -> get_loc_rep (Utils.Location.init_loc col row)) columns
  in
  string_of_int row ^ " | " ^ String.concat " | " loc_reps ^ " |"

(** [rows] is the list of rows on a chess board, in order of printing. *)
let rows = [ 8; 7; 6; 5; 4; 3; 2; 1 ]

let get_start_move_record record = record.start

let string_rep board =
  let get_row_rep = row_rep board in
  let row_reps = List.map get_row_rep rows in
  "  ,-------------------------------.\n"
  ^ String.concat "\n  |---+---+---+---+---+---+---+---|\n" row_reps
  ^ "\n  `-------------------------------'\n    A   B   C   D   E   F   G   H "


let promote board location piece_type color= 
  let updated_pieces = List.map (fun piece -> if Piece.Pieces.get_loc piece = location then Piece.Pieces.init piece_type color location else piece) board.pieces_on_board in
  ({pieces_on_board = updated_pieces; 
    captured_by_white = board.captured_by_white;
    captured_by_black = board.captured_by_black;
    moves = board.moves;
  })

let move_piece_castle board start finish =
  match piece_at_loc board.pieces_on_board start with
  | None -> raise Invalid_move
  | Some piece ->
    let captured_board, captured_a_piece = capture_piece board finish in
    let pieces_on_board =
      move_piece_list captured_board.pieces_on_board start finish
    in
    let new_move_record =
      {piece; start; finish; is_check = in_check board (Piece.Pieces.get_color piece); captured_a_piece; castled=true; promoted=false}
    in
    ( {
        pieces_on_board;
        captured_by_white = captured_board.captured_by_white;
        captured_by_black = captured_board.captured_by_black;
        moves = new_move_record :: captured_board.moves;
      },
      new_move_record )