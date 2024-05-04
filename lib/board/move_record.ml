(* @author Aidan McNay (acm289) *)

type t = {
  piece : Piece.Pieces.t;
  start : Utils.Location.t;
  finish : Utils.Location.t;
  is_check : bool;
  capture : bool;
  castled : bool;
  promoted : Piece.Types.piece_type option;
}

let gen_record piece start finish is_check capture castled promoted =
  { piece; start; finish; is_check; capture; castled; promoted }

let get_piece { piece; _ } = piece
let get_start { start; _ } = start
let get_finish { finish; _ } = finish
let was_check { is_check; _ } = is_check
let was_capture { capture; _ } = capture
let was_castle { castled; _ } = castled
let was_promotion { promoted; _ } = promoted
