(* @author Aidan McNay (acm289) *)

type clicks = {
  locations : Utils.Location.t array;
  num_logged : int;
}
(** The type of a two-deep history of clicks. *)
(* RI: [locations] has exactly two entries, and [num_logged] can never go above
   2. *)

(** [start_clicks] is the starting state for a click history. *)
let empty_clicks =
  { locations = Array.make 2 (Utils.Location.init_loc 'A' 1); num_logged = 0 }

(** [set_click clicks_ref entry loc] sets entry [entry] in [clicks_ref] to
    [loc]. *)
let set_click clicks_ref entry loc = !clicks_ref.locations.(entry) <- loc

(** [get_click clicks_ref entry] gets entry [entry] in [clicks_ref]. *)
let get_click clicks_ref entry = !clicks_ref.locations.(entry)

(** [incr_num clicks_ref] increments the indicator of how many clicks are stored
    in [clicks_ref]. *)
let incr_num clicks_ref =
  let locations = !clicks_ref.locations in
  let num_logged = !clicks_ref.num_logged in
  clicks_ref := { locations; num_logged = num_logged + 1 }

(** [make_click clicks_ref loc] makes a click at [loc] in [clicks_ref], and
    evaluates to whether a complete move has been made. Raises
    [Failure "Click Buffer Not Cleared"] if the buffer is already full. *)
let make_click clicks_ref loc =
  let num_logged = !clicks_ref.num_logged in
  match num_logged with
  | 0 | 1 ->
      set_click clicks_ref num_logged loc;
      incr_num clicks_ref;
      num_logged = 1
  | _ -> failwith "Click Buffer Not Cleared"

(** [reset_clicks clicks_ref] resets [clicks_ref] to have no history. *)
let reset_click clicks_ref = clicks_ref := empty_clicks

type t = {
  color : Piece.Types.color;
  board_ref : Board.Chessboard.t ref;
  clicks_made : clicks ref;
  popup_ref : Bogue.Layout.t ref;
}

let init board_ref color =
  {
    color;
    board_ref;
    clicks_made = ref empty_clicks;
    popup_ref = ref (Bogue.Layout.empty ~w:1 ~h:1 ());
  }

let register_popup_layout tracker layout = tracker.popup_ref := layout

let log_click tracker loc =
  if Turn.curr_turn () <> tracker.color then ()
  else
    try
      let complete_move = make_click tracker.clicks_made loc in
      if complete_move then (
        let loc1 = get_click tracker.clicks_made 0 in
        let loc2 = get_click tracker.clicks_made 1 in
        reset_click tracker.clicks_made;
        let new_game =
          Board.Chessboard.move_piece !(tracker.board_ref) tracker.color loc1
            loc2
        in
        tracker.board_ref := new_game;
        Turn.make_move tracker.color)
      else ()
    with Board.Chessboard.Invalid_move ->
      Turn.call_callbacks (Some tracker.color);
      Bogue.Popup.info "Whoops - that's not a valid move!" !(tracker.popup_ref)

let selected tracker loc =
  if !(tracker.clicks_made).num_logged = 0 then false
  else loc = get_click tracker.clicks_made 0
