(* @author Aidan McNay (acm289) *)

let bishop_valid_moves loc color presence_list =
  let curr_moves_list = [] in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Up; Right ]
      curr_moves_list
  in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Up; Left ]
      curr_moves_list
  in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Down; Right ]
      curr_moves_list
  in
  let curr_moves_list =
    Types.get_dir_moves loc color presence_list
      Utils.Move.[ Down; Left ]
      curr_moves_list
  in
  curr_moves_list

let metadata : Types.piece_metadata =
  { points = 3; get_valid_moves = bishop_valid_moves }
