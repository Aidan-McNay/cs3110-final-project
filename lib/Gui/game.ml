(* @author Aidan McNay (acm289) *)

(** [cornell] is Cornell's shade of red. *)
let cornell = Bogue.Draw.opaque (Bogue.Draw.find_color "#B31B1B")

(** [light_cornell] is a paler version of Cornell's shade of red. *)
let light_cornell = Bogue.Draw.opaque (Bogue.Draw.find_color "#FFA6A6")

(** [curr_board] is the current chess board we're using. *)
let curr_board = ref Board.Chessboard.initial

(** [update_board loc1 loc2] updates the board by moving a piece from [loc1] to
    [loc2]. *)
let update_board loc1 loc2 =
  let new_game = fst (Board.Chessboard.move_piece !curr_board loc1 loc2) in
  curr_board := new_game

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

(** [row_layout row] is the row layout for [row]. *)
let row_layout row =
  let cols = "ABCDEFGH" in
  let row_arr = Array.make 8 (Bogue.Widget.empty ~w:1 ~h:1 ()) in
  for col_idx = 0 to 7 do
    let col = cols.[col_idx] in
    let loc = Utils.Location.init_loc col row in
    let widget = fst (make_widget loc) in
    row_arr.(col_idx) <- widget
  done;
  Bogue.Layout.flat_of_w ~sep:0 ~align:Bogue.Draw.Center ~scale_content:true
    (Array.to_list row_arr)

(** [board_border] is the layout border that goes around the board layout. *)
let board_border =
  let line = Bogue.Style.mk_line ~color:cornell ~width:5 () in
  let border_style = Bogue.Style.of_border (Bogue.Style.mk_border line) in
  Bogue.Layout.style_bg border_style

(** [board_layout ()] gets the layout of the current chess board, as well as an
    action to update them all. *)
let board_layout () =
  ignore board_border;
  let row_layouts = Array.make 8 (Bogue.Layout.empty ~w:1 ~h:1 ()) in
  for row = 1 to 8 do
    row_layouts.(row - 1) <- row_layout row
  done;
  let layout =
    Bogue.Layout.tower ~sep:0 ~align:Bogue.Draw.Max ~scale_content:true
      (List.rev (Array.to_list row_layouts))
  in
  let update_action () =
    let row_layouts = Array.make 8 (Bogue.Layout.empty ~w:1 ~h:1 ()) in
    for row = 1 to 8 do
      row_layouts.(row - 1) <- row_layout row
    done;
    let new_board_layout =
      Bogue.Layout.tower ~sep:0 ~align:Bogue.Draw.Center ~scale_content:true
        (List.rev (Array.to_list row_layouts))
    in
    Bogue.Layout.set_rooms layout [ new_board_layout ]
  in
  (layout, update_action)

let prompt_text =
  [
    "A valid chess move is in the form of '[C1][I1] [C2][I2]'";
    " - C1 and C2 is a char between A and H.";
    " - I1 and I2 are ints between 1 and 8.";
    "An example usage for a first move would be 'D7 D6'.";
  ]

(** [prompt_layout] is the layout for prompting the user. *)
let prompt_layout =
  Bogue.Layout.tower_of_w ~sep:0 ~align:Bogue.Draw.Center
    (List.map (Bogue.Widget.label ~size:12) prompt_text)

(** [input_button] is the button the user uses to input moves. *)
let input_button =
  let input_button_label = Bogue.Label.create ~size:20 "Move!" in
  let input_button_bg = Bogue.Style.Solid light_cornell in
  Bogue.Widget.button ~fg:cornell ~border_radius:10 ~border_color:cornell
    ~label:input_button_label ~bg_off:input_button_bg ~bg_on:input_button_bg
    ~bg_over:(Some input_button_bg) "N/A"

(** [input_text] is the text-input widget where users enter moves. *)
let input_text = Bogue.Widget.text_input ~prompt:"Next move" ()

(** [input_text_backgroun] is the background for the text input layout. *)
let input_text_background =
  let line = Bogue.Style.mk_line ~color:cornell ~width:2 () in
  let border_style =
    Bogue.Style.of_border (Bogue.Style.mk_border ~radius:5 line)
  in
  let input_text_style =
    Bogue.Style.with_bg
      (Bogue.Style.Solid (Bogue.Draw.opaque Bogue.Draw.white))
      border_style
  in
  Bogue.Layout.style_bg input_text_style

(** [input_layout update_action] is the layout for user input, updating with the
    board with [update_action]. *)
let input_layout update_action =
  let text_layout =
    Bogue.Layout.resident ~w:300 ~background:input_text_background input_text
  in
  let button_layout = Bogue.Layout.resident ~w:100 input_button in
  let button_update _ =
    let move_str = Bogue.Widget.get_text input_text in
    let loc1, loc2 = Parse.get_locs move_str in
    update_board loc1 loc2;
    update_action ();
    Bogue.Widget.set_text input_text ""
  in
  Bogue.Widget.on_button_release ~release:button_update input_button;
  Bogue.Layout.flat ~sep:10 ~align:Bogue.Draw.Center
    [ text_layout; button_layout ]

let game_layout () =
  ignore update_board;
  let chessboard_layout, update_action = board_layout () in
  let layout =
    Bogue.Layout.tower ~sep:0 ~align:Bogue.Draw.Center
      [
        title_layout;
        chessboard_layout;
        prompt_layout;
        input_layout update_action;
      ]
  in
  Bogue.Layout.disable_resize layout;
  layout
