(* @author Aidan McNay (acm289) *)

(** [add_front_move loc color presence_list moves_list] adds the move in front
    of a pawn with location [loc] and color [color] on a board with pieces at
    [presence_list] to [moves_list], when valid. *)
let add_front_move loc color presence_list moves_list =
  try
    let prospective_move =
      match color with
      | Types.White -> Utils.Move.Up
      | Types.Black -> Utils.Move.Down
    in
    let new_loc = Utils.Location.apply_move loc prospective_move in
    match Types.color_present presence_list new_loc with
    | Some _ -> moves_list
    | None -> [ prospective_move ] :: moves_list
  with Utils.Location.Off_board -> moves_list

(** [add_capture_move loc color presence_list dir moves_list] adds the move
    where a pawn with location [loc] and color [color] captures a piece in
    direction [dir] (Left or Right) on a board with pieces at [presence_list] to
    [moves_list], when valid. *)
let add_capture_move loc color presence_list dir moves_list =
  try
    let prospective_moves =
      match color with
      | Types.White -> [ Utils.Move.Up; dir ]
      | Types.Black -> [ Utils.Move.Down; dir ]
    in
    let new_loc = Utils.Location.apply_moves loc prospective_moves in
    match Types.color_present presence_list new_loc with
    | Some target_color ->
        if color <> target_color then prospective_moves :: moves_list
        else moves_list
    | None -> moves_list
  with Utils.Location.Off_board -> moves_list

let pawn_valid_moves loc color presence_list =
  let curr_moves_list = [] in
  let curr_moves_list =
    add_front_move loc color presence_list curr_moves_list
  in
  let curr_moves_list =
    add_capture_move loc color presence_list Utils.Move.Left curr_moves_list
  in
  let curr_moves_list =
    add_capture_move loc color presence_list Utils.Move.Right curr_moves_list
  in
  curr_moves_list

let metadata : Types.piece_metadata =
  { points = 1; get_valid_moves = pawn_valid_moves }
