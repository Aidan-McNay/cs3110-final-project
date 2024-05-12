(* @author Aidan McNay (acm289) *)

(** [use_terminal_to_prompt] is whether to use the terminal to prompt for
    promotion, instead of defaulting to a Queen. *)
let use_terminal_to_prompt = ref false

let use_terminal () = use_terminal_to_prompt := true

(** [prompt_options] are the options for a user to input for promotion. *)
let prompt_options = "\n - Queen\n - Rook\n - Bishop\n - Knight\n"

let prompt_promotion color =
  if Bool.not !use_terminal_to_prompt then Piece.Types.Queen
  else
    let color_str =
      match color with
      | Piece.Types.White -> "White"
      | Piece.Types.Black -> "Black"
    in
    Printf.printf "What should %s promote to?" color_str;
    print_string prompt_options;
    let rec get_resp () =
      let response = read_line () in
      match String.lowercase_ascii response with
      | "queen" -> Piece.Types.Queen
      | "rook" -> Piece.Types.Rook
      | "bishop" -> Piece.Types.Bishop
      | "knight" -> Piece.Types.Knight
      | _ ->
          print_endline "Oops - couldn't recognize that piece! Try again";
          get_resp ()
    in
    get_resp ()
