(* @author Aidan McNay (acm289) *)

exception Invalid_input of string
(** Raised if a user's input isn't in a valid form for a chess move. *)

val get_locs : string -> Utils.Location.t * Utils.Location.t
(** [get_locs input] gets the two indicated locations based on [input]. *)
