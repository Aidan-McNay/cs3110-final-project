(* @author Aidan McNay (acm289) *)

(** [cornell] is Cornell's shade of red. *)
let cornell = Bogue.Draw.opaque (Bogue.Draw.find_color "#B31B1B")

(** [light_cornell] is a paler version of Cornell's shade of red. *)
let light_cornell = Bogue.Draw.opaque (Bogue.Draw.find_color "#FFA6A6")

(** [curr_board] is the current chess board we're using. *)
let curr_board = ref Board.Chessboard.initial

(** [put_in_check ()] is whether the last move on [curr_board] put the other
    player in check. *)
let put_in_check () =
  let last_move = Board.Chessboard.last_move !curr_board in
  Board.Move_record.was_check last_move

(** [popup err_msg layout] displayes [err_msg] on a popup on [layout]. *)
let popup =
  Bogue.Popup.info ~w:200 ~h:100 ~button:"Ok" ~button_w:50 ~button_h:40

let () =
  ignore popup;
  ignore light_cornell

(** [make_widget loc tracker] is a widget for [loc] that logs clicks with
    [tracker]. *)
let make_widget loc tracker =
  let bg = Utils.Location.color_of_loc loc in
  let widget =
    Board.Chessboard.image_at_loc !curr_board loc bg
      ~selected:(Move_tracker.selected tracker loc)
  in
  Bogue.Widget.on_click
    ~click:(fun _ -> Move_tracker.log_click tracker loc)
    widget;
  widget

(** [make_widget_layout loc tracker] is a layout to represent [loc] on a board,
    updating [tracker] when clicked. *)
let make_widget_layout loc tracker =
  let widget = make_widget loc tracker in
  let widget_layout = Bogue.Layout.resident widget in
  let layout =
    Bogue.Layout.flat ~sep:0 ~align:Bogue.Draw.Center ~margins:0
      [ widget_layout ]
  in
  let rec update_layout () =
    let old_widget_layout = List.hd (Bogue.Layout.get_rooms layout) in
    let new_widget = make_widget loc tracker in
    Bogue.Widget.on_click ~click:(fun _ -> update_layout ()) new_widget;
    let new_widget_layout = Bogue.Layout.resident new_widget in
    Bogue.Layout.(
      setx new_widget_layout (getx old_widget_layout);
      sety new_widget_layout (gety old_widget_layout);
      set_width new_widget_layout (width old_widget_layout);
      set_height new_widget_layout (height old_widget_layout);
      auto_scale new_widget_layout);
    Bogue.Layout.set_rooms layout [ new_widget_layout ];
    Bogue.Layout.auto_scale layout;
    Bogue.Widget.update new_widget
  in
  Bogue.Widget.on_click ~click:(fun _ -> update_layout ()) widget;
  Turn.set_callback None update_layout;
  layout

(** [title_layout ()] is a layout containing the title of the game. *)
let title_layout () =
  let widget =
    Bogue.Widget.label ~align:Bogue.Draw.Center ~size:30 "CheckCamelMate"
  in
  Bogue.Layout.resident widget

(** [row_layout row color tracker] is the row layout for [row] from the
    perspective of [color], with each widget updating [tracker]. *)
let row_layout row color tracker =
  let cols =
    match color with
    | Piece.Types.White -> "ABCDEFGH"
    | Piece.Types.Black -> "HGFEDCBA"
  in
  let row_arr = Array.make 8 (Bogue.Layout.empty ~w:1 ~h:1 ()) in
  for col_idx = 0 to 7 do
    let col = cols.[col_idx] in
    let loc = Utils.Location.init_loc col row in
    row_arr.(col_idx) <- make_widget_layout loc tracker
  done;
  Bogue.Layout.flat ~sep:0 ~align:Bogue.Draw.Center ~scale_content:true
    ~margins:0 (Array.to_list row_arr)

(** [board_border] is the layout border that goes around the board layout. *)
let board_border =
  let line = Bogue.Style.mk_line ~color:cornell ~width:5 () in
  let border_style = Bogue.Style.of_border (Bogue.Style.mk_border line) in
  Bogue.Layout.style_bg border_style

(** [board_layout tracker] gets the layout of the current chess board, with each
    widget updating [tracker]. *)
let board_layout tracker color =
  let row_layouts = Array.make 8 (Bogue.Layout.empty ~w:1 ~h:1 ()) in
  for row = 1 to 8 do
    row_layouts.(row - 1) <- row_layout row color tracker
  done;
  let layout_list =
    match color with
    | Piece.Types.White -> List.rev (Array.to_list row_layouts)
    | Piece.Types.Black -> Array.to_list row_layouts
  in
  Bogue.Layout.tower ~sep:0 ~align:Bogue.Draw.Center ~scale_content:true
    ~background:board_border layout_list

(** [prompt_layout color] is the layout for prompting the user based on what
    [color] should do. *)
let prompt_layout color =
  let waiting_prompt =
    match color with
    | Piece.Types.White -> "Waiting for Black..."
    | Piece.Types.Black -> "Waiting for White..."
  in
  let move_prompt = "Click a piece to move!" in
  let check_prompt = "You're in check!" in
  let init_prompt =
    match color with
    | Piece.Types.White -> move_prompt
    | Piece.Types.Black -> waiting_prompt
  in
  let widget = Bogue.Widget.label ~size:20 init_prompt in
  let update_prompt () =
    let new_prompt =
      if Turn.curr_turn () = color then
        if put_in_check () then check_prompt else move_prompt
      else waiting_prompt
    in
    Bogue.Widget.set_text widget new_prompt;
    Bogue.Widget.update widget
  in
  Turn.set_callback None update_prompt;
  Bogue.Layout.tower_of_w ~sep:0 ~align:Bogue.Draw.Center ~w:200 [ widget ]

let game_layout color =
  let tracker = Move_tracker.init curr_board color in
  let chessboard_layout = board_layout tracker color in
  let layout =
    Bogue.Layout.tower ~sep:0 ~align:Bogue.Draw.Center
      [ title_layout (); chessboard_layout; prompt_layout color ]
  in
  Bogue.Layout.disable_resize layout;
  Move_tracker.register_popup_layout tracker layout;
  layout
