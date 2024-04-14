(* @author Aidan McNay (acm289) *)

(** [cornell] is Cornell's shade of red (Carnellian). *)
let cornell = Bogue.Draw.opaque (Bogue.Draw.find_color "#B31B1B")

(** [border_line] is the border for our image boxes. *)
let border_line = Bogue.Style.mk_line ~color:cornell ~width:2 ()

let widget_of_image board loc bg =
  let img_bg = Board.Chessboard.image_at_loc board loc bg in
  let box_style =
    Bogue.Style.with_border
      (Bogue.Style.mk_border ~radius:0 border_line)
      (Bogue.Style.of_bg img_bg)
  in
  Bogue.Widget.box ~style:box_style ()
