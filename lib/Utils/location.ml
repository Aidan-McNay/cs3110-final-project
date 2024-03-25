(* @author Aidan McNay (acm289) *)

type t = {
  column : int;
  row : int;
}
(* AF: The record [{column = c; row = r}] represents the location on a chess
   board with row [r] and column corresponding to [c] with the A1Z26 cipher.
   Example: [{column = 3; row = 4} represents the location C4. ] *)
(* RI: The location must be a valid chess board location; [row] and [column]
   must be between 1 and 8, inclusive. *)

exception Off_board

(** [on_board t] is [t]. Raises: [Off_board] if [t] isn't a valid chess board
    location, defined by the representation invariant. *)
let on_board { column; row } =
  if column < 1 || column > 8 then raise Off_board
  else if row < 1 || row > 8 then raise Off_board
  else { column; row }

(** [int_of_char c] is [c] converted in using the A1Z26 cipher. *)
let int_of_char c = Char.code c - 64

(** [char_of_int v] is [v] converted out using the A1Z26 cipher. Requires: [v]
    is a number in the range 1-26, inclusive. *)
let char_of_int v = Char.chr (v + 64)

let init_loc c r =
  let loc = { column = int_of_char (Char.uppercase_ascii c); row = r } in
  on_board loc

let get_col loc = char_of_int loc.column
let get_row loc = loc.row

let apply_move { column = c; row = r } move =
  match move with
  | Move.Up -> on_board { column = c; row = r + 1 }
  | Move.Down -> on_board { column = c; row = r - 1 }
  | Move.Left -> on_board { column = c - 1; row = r }
  | Move.Right -> on_board { column = c + 1; row = r }

let apply_moves loc moves = List.fold_left apply_move loc moves
let str_of_loc loc = String.make 1 (get_col loc) ^ string_of_int (get_row loc)
