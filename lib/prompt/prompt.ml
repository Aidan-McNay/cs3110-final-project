(* @author Aidan McNay (acm289) *)

(** [black_layout] is a reference to the black player's layout, if it exists. *)
let black_layout : Bogue.Layout.t option ref = ref None

(** [white_layout] is a reference to the black player's layout, if it exists. *)
let white_layout : Bogue.Layout.t option ref = ref None

let set_layout color layout =
  match color with
  | Piece.Types.White -> white_layout := Some layout
  | Piece.Types.Black -> black_layout := Some layout

(** [response] is a reference to the player's response, if any. *)
let response : Piece.Types.piece_type option ref = ref None

(** [response_mutex] is a mutex for protecting concurrent access to [response]. *)
let response_mutex = Mutex.create ()

(** [set_resp piece_type] sets the user's response to [piece_type]. *)
let set_resp piece_type =
  Mutex.lock response_mutex;
  response := Some piece_type;
  Mutex.unlock response_mutex

(** [get_resp ()] gets the response from the user, waiting until they provide
    one. *)
let rec get_resp () =
  Mutex.lock response_mutex;
  let curr_resp = !response in
  response := None;
  Mutex.unlock response_mutex;
  match curr_resp with
  | None -> get_resp ()
  | Some piece_type -> piece_type

(** [slide_in ~dst content buttons] is a popup with [buttons] on top of
    [content], optionally attaching to [dst]. Source: Bogue source code, in
    lib/b_popup.ml at line 127. Taken from
    https://github.com/sanette/bogue/blob/b307b736609e19ba6fee688cfed761ce9425a2e1/lib/b_popup.ml#L127
    on May 6th, 2024. *)
let slide_in ~dst content buttons =
  let style =
    Bogue.Style.(
      create
        ~border:(mk_border (mk_line ~color:Bogue.Draw.(opaque grey) ()))
        ~shadow:(mk_shadow ())
        ~background:(Solid Bogue.Draw.(opaque (pale grey)))
        ())
  in
  let background = Bogue.Layout.style_bg style in
  let popup =
    Bogue.Layout.tower ~align:Bogue.Draw.Center ~background [ content; buttons ]
  in
  let screen =
    Bogue.Popup.attach
      ~bg:Bogue.Draw.(set_alpha 200 (pale Bogue.Draw.grey))
      dst popup
  in
  (popup, screen)

(** [mk_button text] makes a button that displays [text]. *)
let mk_button text = Bogue.Widget.button ~border_radius:3 text

(** [prompt_menu ()] is a new prompt menu for promotion. *)
let prompt_menu house =
  let queen_btn = mk_button "Queen" in
  let rook_btn = mk_button "Rook" in
  let bishop_btn = mk_button "Bishop" in
  let knight_btn = mk_button "Knight" in
  let prompt =
    Bogue.Widget.text_display "What do you want to promote to?"
    |> Bogue.Layout.resident
  in
  let buttons =
    Bogue.Layout.(
      flat_of_w ~sep:0 [ queen_btn; rook_btn; bishop_btn; knight_btn ])
  in
  let curr_rooms = Bogue.Layout.get_rooms house in
  let popup, screen = slide_in ~dst:house prompt buttons in
  let close () =
    let open Bogue.Layout in
    let _ =
      Bogue.Timeout.add 300 (fun () -> Bogue.Layout.set_rooms house curr_rooms)
    in
    fade_out ~hide:true popup;
    fade_out ~hide:true screen
  in
  let set_piece piece_type _ =
    close ();
    set_resp piece_type
  in
  Bogue.Widget.on_button_release
    ~release:(set_piece Piece.Types.Queen)
    queen_btn;
  Bogue.Widget.on_button_release ~release:(set_piece Piece.Types.Rook) rook_btn;
  Bogue.Widget.on_button_release
    ~release:(set_piece Piece.Types.Bishop)
    bishop_btn;
  Bogue.Widget.on_button_release
    ~release:(set_piece Piece.Types.Knight)
    knight_btn

let prompt_promotion color =
  let prompt_layout =
    match color with
    | Piece.Types.White -> !white_layout
    | Piece.Types.Black -> !black_layout
  in
  match prompt_layout with
  | None -> Piece.Types.Queen
  | Some l ->
      let house_layout = Bogue.Layout.top_house l in
      prompt_menu house_layout;
      get_resp ()
