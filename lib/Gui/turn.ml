(* @author Aidan McNay (acm289) *)

(** The current turn's color - we start on White *)
let curr_turn_color = ref Piece.Types.White

let curr_turn () = !curr_turn_color

(** [white_callbacks] points to the functions to call when white receives the
    turn. *)
let white_callbacks = ref []

(** [black_callbacks] points to the functions to call when white receives the
    turn. *)
let black_callbacks = ref []

let set_callback color f =
  match color with
  | Some Piece.Types.White -> white_callbacks := f :: !white_callbacks
  | Some Piece.Types.Black -> black_callbacks := f :: !black_callbacks
  | None ->
      black_callbacks := f :: !black_callbacks;
      white_callbacks := f :: !white_callbacks

let call_callbacks color =
  match color with
  | Some Piece.Types.White -> List.iter (fun f -> f ()) !white_callbacks
  | Some Piece.Types.Black -> List.iter (fun f -> f ()) !black_callbacks
  | None ->
      List.iter (fun f -> f ()) !white_callbacks;
      List.iter (fun f -> f ()) !black_callbacks

exception Not_your_turn of Piece.Types.color

let make_move color =
  if color <> !curr_turn_color then raise (Not_your_turn color)
  else (
    (match color with
    | Piece.Types.White -> curr_turn_color := Piece.Types.Black
    | Piece.Types.Black -> curr_turn_color := Piece.Types.White);
    call_callbacks (Some color))
