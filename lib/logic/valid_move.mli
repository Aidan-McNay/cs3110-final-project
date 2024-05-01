val start :unit -> unit
(** [start] is the infinite sequence of the chess game*)


exception Has_Won of Piece.Pieces.color
(** [Has_Won] is an exception that specifies that this color has won*)

val make_move : Board.Chessboard.t -> Piece.Types.color -> Utils.Location.t -> Utils.Location.t -> Board.Chessboard.t
(** [make_move] will make a move if the move is a legal move according to the rules outlined in chess which include capturing, 
    staying out of check, promotion, and castling
    caveats: 
    - Promotion will currently just automatically promote the user's piece to a Queen
    - Castling is currently not working - I spent a decent amount of time trying to figure out why but I'm still running into bugs where it 
    will occasionally work.contents

    If a player puts the other player in checkmate make_move will throw the error Has_Won of the player color who has won

    Sorry for the wordy doc, but wanted this to be verbose :) HMU if anything is being weird beyond castling 
*)