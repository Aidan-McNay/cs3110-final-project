(* @author Aidan McNay (acm289) *)

(** [curr_board] is the current chess board we're using. *)
let curr_board = ref Board.Chessboard.initial

(** [make_widget loc] is a widget to represent [loc] on a board, as well as an
    action to update it from. *)
let make_widget loc =
  let bg = Utils.Location.color_of_loc loc in
  let widget = Board.Chessboard.image_at_loc !curr_board loc bg in
  (widget, bg)

(** [title_layout] is the layout containing the title of the game. *)
let title_layout =
  let widget =
    Bogue.Widget.label ~align:Bogue.Draw.Center ~size:30 "CheckCamelMate"
  in
  Bogue.Layout.resident widget

let get_layout () =
  let cols = "HGFEDCBA" in
  let row_layouts = Array.make 8 (Bogue.Layout.empty ~w:1 ~h:1 ()) in
  for row = 1 to 8 do
    let row_arr = Array.make 8 (Bogue.Widget.empty ~w:1 ~h:1 ()) in
    for col_idx = 0 to 7 do
      let col = cols.[col_idx] in
      let loc = Utils.Location.init_loc col row in
      let widget = fst (make_widget loc) in
      row_arr.(col_idx) <- widget
    done;
    let row_layout =
      Bogue.Layout.flat_of_w ~sep:0 ~align:Bogue.Draw.Center
        (Array.to_list row_arr)
    in
    row_layouts.(row - 1) <- row_layout
  done;
  Bogue.Layout.tower ~sep:0 ~align:Bogue.Draw.Center
    (title_layout :: Array.to_list row_layouts)
