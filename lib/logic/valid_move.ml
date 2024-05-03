(* @author Aidan McNay (acm289), Henry Toll (hht26) *)

exception Invalid_input of string
(** Raised if a user's input isn't in a valid form for a chess move*)

(**[get_input] is a tuple containing two elements each being tuples with two
   elements.contents get_input will take a input like "A6 B7" and output a tuple
   of (("A", 6),("B",7)) where "A","B" are chars and 6,7 are ints. Raises:
   [Invalid_input] if the input isn't a valid string of the provided format*)
let get_input string =
  let parts = String.split_on_char ' ' string in
  (* Split input into parts based on space *)
  match parts with
  | [ first; second ] ->
      (* Validate both parts are correctly formatted e.g., "A6" *)
      let validate_part part =
        if
          String.length part <> 2
          || (not
                (Char.code part.[0] >= Char.code 'A'
                 && Char.code part.[0] <= Char.code 'Z'
                || Char.code part.[0] >= Char.code 'a'
                   && Char.code part.[0] <= Char.code 'z'))
          || not (part.[1] >= '0' && part.[1] <= '9')
        then
          raise
            (Invalid_input
               "Each part must be a single character followed by a single digit")
      in
      validate_part first;
      validate_part second;
      let char1 = first.[0] in
      let num1 = int_of_string (String.sub first 1 (String.length first - 1)) in
      let char2 = second.[0] in
      let num2 =
        int_of_string (String.sub second 1 (String.length second - 1))
      in
      ((char1, num1), (char2, num2))
      (* Return the tuple *)
  | _ ->
      raise
        (Invalid_input "Input must consist of two parts separated by a space")

let unwrap x =
  match x with
  | Some x -> x
  | None -> raise Board.Chessboard.Invalid_move

(* let in_check board *)
exception In_Check

(** Finish castling Add a castle updated to move_record -> add_update Add a
    check update to move_record Add promotions ability to the player Add
    promotion field to the move_record Finishing Checkmate Add special pawn
    movement Add special pawn movements to the move_record *)

(** [check]*)
let check_check game start_location end_location color =
  let game2 = Board.Chessboard.move_piece game start_location end_location in
  if Board.Chessboard.in_check (fst game2) (color : Piece.Pieces.color) then
    fst game2
  else raise In_Check

(* let has_moved_from_start game location = let movement = List.filter (fun
   record -> Board.Chessboard.get_start_move_record record = location)
   (Board.Chessboard.move_history game) in if List.is_empty movement then false
   else true *)

(* let caste_check_castle1 game loc1 = ignore loc1; if not (has_moved_from_start
   game (Utils.Location.init_loc 'e' 1)) then if not (has_moved_from_start game
   (Utils.Location.init_loc 'h' 1)) then let game = check_check game
   (Utils.Location.init_loc 'e' 1) (Utils.Location.init_loc 'f' 1) (White :
   Piece.Pieces.color) in let game = check_check game (Utils.Location.init_loc
   'f' 1) (Utils.Location.init_loc 'g' 1) (White : Piece.Pieces.color) in ( fst
   (Board.Chessboard.move_piece_castle game (Utils.Location.init_loc 'h' 1)
   (Utils.Location.init_loc 'f' 1)), true ) else raise
   Board.Chessboard.Invalid_move else raise Board.Chessboard.Invalid_move *)

(* let caste_check_castle2 game loc1 = ignore loc1; if not (has_moved_from_start
   game (Utils.Location.init_loc 'e' 1)) then if not (has_moved_from_start game
   (Utils.Location.init_loc 'a' 1)) then let game = check_check game
   (Utils.Location.init_loc 'a' 1) (Utils.Location.init_loc 'b' 1) (White :
   Piece.Pieces.color) in let game = check_check game (Utils.Location.init_loc
   'e' 1) (Utils.Location.init_loc 'd' 1) (White : Piece.Pieces.color) in let
   game = check_check game (Utils.Location.init_loc 'd' 1)
   (Utils.Location.init_loc 'c' 1) (White : Piece.Pieces.color) in ( fst
   (Board.Chessboard.move_piece_castle game (Utils.Location.init_loc 'a' 1)
   (Utils.Location.init_loc 'd' 1)), true ) else raise
   Board.Chessboard.Invalid_move else raise Board.Chessboard.Invalid_move *)

(* let caste_check_castle3 game loc8 = ignore loc8; if not (has_moved_from_start
   game (Utils.Location.init_loc 'e' 8)) then if not (has_moved_from_start game
   (Utils.Location.init_loc 'h' 8)) then let game = check_check game
   (Utils.Location.init_loc 'e' 8) (Utils.Location.init_loc 'f' 8) (White :
   Piece.Pieces.color) in let game = check_check game (Utils.Location.init_loc
   'f' 8) (Utils.Location.init_loc 'g' 8) (White : Piece.Pieces.color) in ( fst
   (Board.Chessboard.move_piece_castle game (Utils.Location.init_loc 'h' 8)
   (Utils.Location.init_loc 'f' 8)), true ) else raise
   Board.Chessboard.Invalid_move else raise Board.Chessboard.Invalid_move *)

(* let caste_check_castle4 game loc8 = ignore loc8; if not (has_moved_from_start
   game (Utils.Location.init_loc 'e' 8)) then if not (has_moved_from_start game
   (Utils.Location.init_loc 'a' 8)) then let game = check_check game
   (Utils.Location.init_loc 'a' 8) (Utils.Location.init_loc 'b' 8) (White :
   Piece.Pieces.color) in let game = check_check game (Utils.Location.init_loc
   'e' 8) (Utils.Location.init_loc 'd' 8) (White : Piece.Pieces.color) in let
   game = check_check game (Utils.Location.init_loc 'd' 8)
   (Utils.Location.init_loc 'c' 8) (White : Piece.Pieces.color) in ( fst
   (Board.Chessboard.move_piece_castle game (Utils.Location.init_loc 'a' 8)
   (Utils.Location.init_loc 'd' 8)), true ) else raise
   Board.Chessboard.Invalid_move else raise Board.Chessboard.Invalid_move *)

(** Color seperates it into two options of location) White - > e1 g1 or e1 c1
    Black -> e8 g8 or e8 c8

    Each of these make sure the king has never moved and the rook has never
    moved Make sure that the king isn't in check, or that any spot that it has
    to move thorugh is in check

    aka for white that would mean for e1 g1

    e1 isn't in check f1 isn't in check and g1 isn't in check

    e1 c1 means e1, d1, c1

    also means no piece there has king or ro moved *)
(* let castle_check game start_location end_location = match (start_location,
   end_location) with | ('e', 1), ('g', 1) -> caste_check_castle1 game
   (Utils.Location.init_loc 'g' 1) | ('e', 1), ('c', 1) -> caste_check_castle2
   game (Utils.Location.init_loc 'c' 1) | ('e', 8), ('g', 8) ->
   caste_check_castle3 game (Utils.Location.init_loc 'g' 8) | ('e', 8), ('c', 8)
   -> caste_check_castle4 game (Utils.Location.init_loc 'c' 8) | _, _ -> (game,
   false) *)

let promote_queen board location color =
  let board = Board.Chessboard.promote board location Piece.Types.Queen color in
  board

let rec promote board location color =
  try
    let () =
      print_endline
        "What would you like to promote to? - Rook, Queen, Bishop, Knight"
    in
    let input = read_line () in
    match String.lowercase_ascii input with
    | "rook" -> Board.Chessboard.promote board location Piece.Types.Rook color
    | "queen" -> Board.Chessboard.promote board location Piece.Types.Queen color
    | "bishop" ->
        Board.Chessboard.promote board location Piece.Types.Bishop color
    | "knight" ->
        Board.Chessboard.promote board location Piece.Types.Knight color
    | _ -> raise (Invalid_input "That is not a type of piece")
  with Invalid_input x ->
    let () = print_endline x in
    promote board location color

(** [move_white game] is a tuple containg the updated Board.Chessboard.t and the
    Board.Chessboard.move_record which is updated by prompting the white player
    to enter their next move, reads the input, and attempts to update the game
    state with the white player's move. The function recursively calls itself
    until a valid move is made.

    Requires: The [game] parameter must be a valid Board.Chessboard.t *)
let rec move_white game =
  try
    let () = print_endline "What is Whites next move?" in
    let input = read_line () in
    try
      let movements = get_input input in
      let start_location =
        Utils.Location.init_loc (fst (fst movements)) (snd (fst movements))
      in
      let end_location =
        Utils.Location.init_loc (fst (snd movements)) (snd (snd movements))
      in
      let piece =
        unwrap
          (Board.Chessboard.piece_at_loc
             (Board.Chessboard.get_pieces_on_board game)
             start_location)
      in
      if Piece.Pieces.get_color piece = (White : Piece.Pieces.color) then
        let game =
          check_check game start_location end_location
            (White : Piece.Pieces.color)
        in
        let check_promotion =
          Board.Chessboard.check_board_for_promotion game
            (White : Piece.Pieces.color)
        in
        if snd check_promotion then
          promote_queen game (fst check_promotion) (White : Piece.Pieces.color)
        else game
      else raise Board.Chessboard.Invalid_move
    with Invalid_input x ->
      let () = print_endline x in
      move_white game
  with
  | Board.Chessboard.Invalid_move ->
      let () = print_endline "That is not a valid move for White" in
      move_white game
  | In_Check ->
      let () =
        print_endline
          "That is not a valid move for White as you would be in check"
      in
      move_white game

(** [move_black game] is a tuple containg the updated Board.Chessboard.t and the
    Board.Chessboard.move_record which is updated by prompting the black player
    to enter their next move, reads the input, and attempts to update the game
    state with the black player's move. The function recursively calls itself
    until a valid move is made. Requires: The [game] parameter must be a valid
    Board.Chessboard.t *)
let rec move_black game =
  try
    let () = print_endline "What is Blacks next move?" in
    let input = read_line () in
    try
      let movements = get_input input in
      let start_location =
        Utils.Location.init_loc (fst (fst movements)) (snd (fst movements))
      in
      let end_location =
        Utils.Location.init_loc (fst (snd movements)) (snd (snd movements))
      in
      let piece =
        unwrap
          (Board.Chessboard.piece_at_loc
             (Board.Chessboard.get_pieces_on_board game)
             start_location)
      in
      if Piece.Pieces.get_color piece = (Black : Piece.Pieces.color) then
        let game =
          check_check game start_location end_location
            (Black : Piece.Pieces.color)
        in
        let check_promotion =
          Board.Chessboard.check_board_for_promotion game
            (Black : Piece.Pieces.color)
        in
        if snd check_promotion then
          promote game (fst check_promotion) (Black : Piece.Pieces.color)
        else game
      else raise Board.Chessboard.Invalid_move
    with Invalid_input x ->
      let () = print_endline x in
      move_black game
  with
  | Board.Chessboard.Invalid_move ->
      let () = print_endline "That is not a valid move for Black" in
      move_black game
  | In_Check ->
      let () =
        print_endline
          "That is not a valid move for Black as you would be in check"
      in
      move_black game

(** [play game] alternates moves between the white and black players in a game
    of chess, updating the game state after each move. It displays the string
    representation of the chessboard after each player makes a move. This
    function recursively calls itself forever - it doesn't have end condition
    yet, it will eventually have a condition that checks if either player is in
    checkmate and will then end the game if one player is.

    Requires: The [game] parameter must be a valid Board.Chessboard.t
    representing the start of a chess game. *)
let rec play game =
  let game = move_white game in
  let () = print_endline (Board.Chessboard.string_rep game) in
  if
    Board.Chessboard.get_in_check_move_record (Board.Chessboard.last_move game)
    && Board.Chessboard.checkmate game (Black : Piece.Types.color)
  then print_endline "White has won"
  else
    let game = move_black game in
    let () = print_endline (Board.Chessboard.string_rep game) in
    if
      Board.Chessboard.get_in_check_move_record
        (Board.Chessboard.last_move game)
      && Board.Chessboard.checkmate game (White : Piece.Types.color)
    then print_endline "Black has won"
    else play game

(* exception Unimplemented *)
(* let make_move board color loc1 loc2 = ignore board; ignore color; ignore
   loc1; ignore loc2; raise Unimplemented *)

let opposite_color (color : Piece.Pieces.color) : Piece.Pieces.color =
  match color with
  | Black -> White
  | White -> Black

exception Has_won of Piece.Pieces.color

let make_move board color loc1 loc2 =
  try
    let start_location = loc1 in
    let end_location = loc2 in
    (* let movements = ( (Utils.Location.get_col loc1, Utils.Location.get_row
       loc1), (Utils.Location.get_col loc2, Utils.Location.get_row loc2) ) in *)
    let piece =
      unwrap
        (Board.Chessboard.piece_at_loc
           (Board.Chessboard.get_pieces_on_board board)
           start_location)
    in
    if Piece.Pieces.get_color piece = (color : Piece.Pieces.color) then
      let board =
        check_check board start_location end_location
          (color : Piece.Pieces.color)
      in
      let check_promotion =
        Board.Chessboard.check_board_for_promotion board
          (color : Piece.Pieces.color)
      in
      if snd check_promotion then
        promote_queen board (fst check_promotion) (color : Piece.Pieces.color)
      else if
        Board.Chessboard.get_in_check_move_record
          (Board.Chessboard.last_move board)
        && Board.Chessboard.checkmate board
             (opposite_color color : Piece.Types.color)
      then raise (Has_won color)
      else board
    else raise Board.Chessboard.Invalid_move
  with
  | Board.Chessboard.Invalid_move -> raise Board.Chessboard.Invalid_move
  | In_Check -> raise Board.Chessboard.Invalid_move

(** [start] initializes a new game of chess with the board in its initial setup
    and starts the game loop by calling [play] with this initial game state. It
    first displays the string representation of the initial chessboard setup
    before calling [play] with the initial chess board setup. *)
let start () =
  let game = Board.Chessboard.initial in
  let () = print_endline (Board.Chessboard.string_rep game) in
  let () = print_endline "" in
  let () =
    print_endline "A valid chess move is in the form of '[C1][I1] [C2][I2]'"
  in
  let () = print_endline " - C1 and C2 is a char between A and H." in
  let () = print_endline " - I1 and I2 are ints between 1 and 8." in
  let () =
    print_endline "An example usage for a first move would be 'D7 D6'."
  in
  let () = print_endline "" in
  play game
