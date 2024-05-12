(* @author Aidan McNay (acm289) *)

val use_terminal : unit -> unit
(** [use_terminal ()] makes all further prompts use the terminal, instead of
    defaulting to a Queen *)

val prompt_promotion : Piece.Types.color -> Piece.Types.piece_type
(** [prompt_promotion color] prompts the [color] player for which piece type
    they'd like to promote to. [prompt_promotion color] will only ever evaluate
    to [Knight], [Bishop], [Rook], or [Queen]. *)
