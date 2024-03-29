(* @author Aidan McNay (acm289) *)

let knight_moves : Utils.Move.moves list =
  Utils.Move.
    [
      [ Up; Up; Left ];
      [ Up; Up; Right ];
      [ Down; Down; Left ];
      [ Down; Down; Right ];
      [ Left; Left; Up ];
      [ Left; Left; Down ];
      [ Right; Right; Up ];
      [ Right; Right; Down ];
    ]

let valid_move piece_color curr_loc presence_list moves =
  try
    let new_loc = Utils.Location.apply_moves curr_loc moves in
    match Types.color_present presence_list new_loc with
    | Some target_color ->
        if piece_color <> target_color then Some moves else None
    | None -> Some moves
  with Utils.Location.Off_board -> None

let knight_valid_moves loc color presence_list =
  let is_valid_move = valid_move color loc presence_list in
  List.filter_map is_valid_move knight_moves

let metadata : Types.piece_metadata =
  { points = 3; get_valid_moves = knight_valid_moves }
