(* @author Aidan McNay (acm289) *)

let king_moves : Utils.Move.moves list =
  Utils.Move.
    [
      [ Up ];
      [ Down ];
      [ Left ];
      [ Right ];
      [ Up; Right ];
      [ Up; Left ];
      [ Down; Right ];
      [ Down; Left ];
    ]

let valid_move piece_color curr_loc presence_list moves =
  try
    let new_loc = Utils.Location.apply_moves curr_loc moves in
    match Types.color_present presence_list new_loc with
    | Some target_color ->
        if piece_color <> target_color then Some moves else None
    | None -> Some moves
  with Utils.Location.Off_board -> None

let king_valid_moves loc color presence_list =
  let is_valid_move = valid_move color loc presence_list in
  List.filter_map is_valid_move king_moves

let metadata : Types.piece_metadata =
  { points = 200; get_valid_moves = king_valid_moves }
