(* @author Aidan McNay (acm289) *)

(** [prompt_options] are the options for a user to input for promotion. *)
let prompt_options = "\n - Queen\n - Rook\n - Bishop\n - Knight\n"

let prompt_promotion color =
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
