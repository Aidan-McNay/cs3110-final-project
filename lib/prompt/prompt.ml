(* @author Aidan McNay (acm289) *)

(** [black_layout] is a reference to the black player's layout, if it exists. *)
let black_layout : Bogue.Layout.t option ref = ref None

(** [white_layout] is a reference to the black player's layout, if it exists. *)
let white_layout : Bogue.Layout.t option ref = ref None

let set_layout color layout =
  match color with
  | Piece.Types.White -> white_layout := Some layout
  | Piece.Types.Black -> black_layout := Some layout

(** [prompt_menu ()] is a new prompt menu for promotion. *)
let prompt_promotion _ = Piece.Types.Queen
