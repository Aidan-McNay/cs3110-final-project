(** Raised if a user's input isn't in a valid form for a chess move*)
exception Invalid_input of string;;

(**[get_input] is a tuple containing two elements each being tuples with two elements.contents
    get_input will take a input like "A6 B7" and output a tuple of (("A", 6),("B",7)) where "A","B" are chars 
    and 6,7 are ints.
    Raises: [Invalid_input] if the input isn't a valid string of the provided format*)
let get_input string =
  let parts = String.split_on_char ' ' string in (* Split input into parts based on space *)
  match parts with
  | [first; second] -> 
      (* Validate both parts are correctly formatted e.g., "A6" *)
      let validate_part part =
        if String.length part <> 2 || not (Char.code part.[0] >= Char.code 'A' && Char.code part.[0] <= Char.code 'Z' || Char.code part.[0] >= Char.code 'a' && Char.code part.[0] <= Char.code 'z') || not (part.[1] >= '0' && part.[1] <= '9') then
          raise (Invalid_input "Each part must be a single character followed by a single digit");
      in
      validate_part first;
      validate_part second;
      let char1 = first.[0] in
      let num1 = int_of_string (String.sub first 1 (String.length first - 1)) in
      let char2 = second.[0] in
      let num2 = int_of_string (String.sub second 1 (String.length second - 1)) in
      ((char1, num1), (char2, num2)) (* Return the tuple *)
  | _ -> raise (Invalid_input "Input must consist of two parts separated by a space")
    
(** [move_white game] is a tuple containg the updated Board.Chessboard.t and 
  the Board.Chessboard.move_record which is updated by prompting the white player 
  to enter their next move, reads the input, and attempts to update the game 
  state with the white player's move. 
  The function recursively calls itself until a valid move is made.

  Requires: The [game] parameter must be a valid Board.Chessboard.t
*)
let rec move_white game = 
  try 
    let () = print_endline("What is Whites next move?") in
    let input = read_line() in
    try 
      let movements = get_input input in
      let start_location = Utils.Location.init_loc (fst (fst movements)) (snd (fst movements)) in
      let end_location = Utils.Location.init_loc (fst (snd movements)) (snd (snd movements)) in
      let game2 = Board.Chessboard.move_piece game start_location end_location in 
      game2
  with  
    | Invalid_input x -> let () = print_endline(x) in move_white game
with
| Board.Chessboard.Invalid_move -> let () = print_endline("That is not a valid move for White") in move_white game

(** [move_black game] is a tuple containg the updated Board.Chessboard.t and 
  the Board.Chessboard.move_record which is updated by prompting the black player 
  to enter their next move, reads the input, and attempts to update the game 
  state with the black player's move. 
  The function recursively calls itself until a valid move is made.
  Requires: The [game] parameter must be a valid Board.Chessboard.t
*)
let rec move_black game = 
  try 
    let () = print_endline("What is Blacks next move?") in
    let input = read_line() in
    try 
      let movements = get_input input in
      let start_location = Utils.Location.init_loc (fst (fst movements)) (snd (fst movements)) in
      let end_location = Utils.Location.init_loc (fst (snd movements)) (snd (snd movements)) in
      let game2 = Board.Chessboard.move_piece game start_location end_location in 
      game2
  with  
    | Invalid_input x -> let () = print_endline(x) in move_black game
with
  | Board.Chessboard.Invalid_move -> let () = print_endline("That is not a valid move for Black") in move_black game

(** [play game] alternates moves between the white and black players in a game of chess, 
    updating the game state after each move. It displays the string representation of the 
    chessboard after each player makes a move. This function recursively calls itself 
    forever - it doesn't have end condition yet, it will eventually have a condition that checks if 
    either player is in checkmate and will then end the game if one player is.

    Requires: The [game] parameter must be a valid Board.Chessboard.t representing the start
     of a chess game. 
*)
let rec play game = 
  let game = move_white game in
  let () = print_endline(Board.Chessboard.string_rep (fst game)) in
  let game = move_black (fst game) in
  let () = print_endline(Board.Chessboard.string_rep (fst game)) in
  play (fst game)
  
(** [start] initializes a new game of chess with the board in its initial setup and 
    starts the game loop by calling [play] with this initial game state. It first displays 
    the string representation of the initial chessboard setup before calling [play] with the 
    initial chess board setup.
*)
let start = 
  let game = Board.Chessboard.initial in
  let () = print_endline(Board.Chessboard.string_rep (game)) in
  let () = print_endline("") in
  let () = print_endline("A valid chess move is in the form of '[C1][I1] [C2][I2]'") in
  let () = print_endline(" - C1 and C2 is a char between A and H.") in
  let () = print_endline(" - I1 and I2 are ints between 1 and 8.") in
  let () = print_endline("An example usage for a first move would be 'D7 D6'.") in
  let () = print_endline("") in
  play game

let () =
  print_endline "Testing Board...";
  print_endline Board.Test.message;
  print_endline "Testing Game...";
  print_endline Game.Test.message;
  print_endline "Testing Piece...";
  print_endline Piece.Test.message;
  start

