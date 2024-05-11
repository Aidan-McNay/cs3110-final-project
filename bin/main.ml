(* @author Aidan McNay (acm289) *)

(** [start] initializes a new game of chess with the board in its initial setup
    and starts the game loop. *)
let start_gui () =
  let board =
    Bogue.Main.of_layouts
      (List.map Gui.Game.game_layout [ Piece.Types.White; Piece.Types.Black ])
  in
  Bogue.Main.run board

let () =
  start_gui ();
  Bogue.Main.quit ()
