(* @author Aidan McNay (acm289) *)

type t = {
  piece_type : Piece.Types.piece_type;
  color : Piece.Types.color;
  start : Utils.Location.t;
  finish : Utils.Location.t;
  is_check : bool;
  capture : bool;
  castled : bool;
  promoted : Piece.Types.piece_type option;
  checkmate : bool;
  alg_not : string;
}

let gen_record piece_type color start finish is_check capture castled promoted
    checkmate alg_not =
  {
    piece_type;
    color;
    start;
    finish;
    is_check;
    capture;
    castled;
    promoted;
    checkmate;
    alg_not;
  }

let get_piece_type { piece_type; _ } = piece_type
let get_start { start; _ } = start
let get_finish { finish; _ } = finish
let was_check { is_check; _ } = is_check
let was_capture { capture; _ } = capture
let was_castle { castled; _ } = castled
let was_promotion { promoted; _ } = promoted
let get_alg_not { alg_not; _ } = alg_not
let get_color { color; _ } = color
let get_checkmate { checkmate; _ } = checkmate
