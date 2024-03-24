(* @author Aidan McNay (acm289) *)

let rook_valid_moves loc color presence_list =
  let curr_moves_list = [] in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Up ]
      curr_moves_list
  in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Left ]
      curr_moves_list
  in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Down ]
      curr_moves_list
  in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Right ]
      curr_moves_list
  in
  curr_moves_list

let metadata : Types.piece_metadata =
  { points = 5; get_valid_moves = rook_valid_moves }
