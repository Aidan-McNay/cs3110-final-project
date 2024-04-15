(* @author Aidan McNay (acm289) *)

exception Invalid_input of string

let get_locs string =
  let parts = String.split_on_char ' ' string in
  (* Split input into parts based on space *)
  match parts with
  | [ first; second ] ->
      (* Validate both parts are correctly formatted e.g., "A6" *)
      let validate_part part =
        if
          String.length part <> 2
          || (not
                (Char.code part.[0] >= Char.code 'A'
                 && Char.code part.[0] <= Char.code 'Z'
                || Char.code part.[0] >= Char.code 'a'
                   && Char.code part.[0] <= Char.code 'z'))
          || not (part.[1] >= '0' && part.[1] <= '9')
        then
          raise
            (Invalid_input
               "Each part must be a single character followed by a single digit")
      in
      validate_part first;
      validate_part second;
      let char1 = first.[0] in
      let num1 = int_of_string (String.sub first 1 (String.length first - 1)) in
      let char2 = second.[0] in
      let num2 =
        int_of_string (String.sub second 1 (String.length second - 1))
      in
      (Utils.Location.init_loc char1 num1, Utils.Location.init_loc char2 num2)
      (* Return the tuple *)
  | _ ->
      raise
        (Invalid_input "Input must consist of two parts separated by a space")
