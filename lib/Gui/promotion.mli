(* @author Aidan McNay (acm289) *)

val set_layout : Piece.Types.color -> Bogue.Layout.t -> unit
(** [set_layout color] sets the layout that should be used for popups when
    [color] is prompted for promotion. *)

val prompt_promotion : Piece.Types.color -> Piece.Types.piece_type
(** [prompt_promotion color] prompts the [color] player for which piece type
    they'd like to promote to. [prompt_promotion color] will only ever evaluate
    to [Knight], [Bishop], [Rook], or [Queen]. *)
