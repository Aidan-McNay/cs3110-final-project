open OUnit2

(** [assert_loc loc col row] asserts that location [loc] has the column [col]
    and row [row]. *)
let assert_loc loc col row =
  assert_equal (Char.uppercase_ascii col)
    (Utils.Location.get_col loc)
    ~printer:(String.make 1);
  assert_equal row (Utils.Location.get_row loc) ~printer:string_of_int

(** [basic_test col row] runs basic tests on a location with column [col] and
    row [row]. *)
let basic_test col row =
  let loc = Utils.Location.init_loc col row in
  assert_loc loc col row

(** [off_board_test col row] checks that the location with column [col] and row
    [row] is off the board. *)
let off_board_test col row =
  assert_raises Utils.Location.Off_board (fun () ->
      Utils.Location.init_loc col row)

(** [move_test init_col init_row move end_col end_row] checks that a location
    formed from column [init_col] and row [init_row] ends up as the location
    with column [end_col] and row [end_row] after being moved by [move]. *)
let move_test init_col init_row move end_col end_row =
  let loc = Utils.Location.init_loc init_col init_row in
  let new_loc = Utils.Location.apply_move loc move in
  assert_loc new_loc end_col end_row

(** [moves_test init_col init_row moves end_col end_row] checks that a location
    formed from column [init_col] and row [init_row] ends up as the location
    with column [end_col] and row [end_row] after being moved by [moves]. *)
let moves_test init_col init_row moves end_col end_row =
  let loc = Utils.Location.init_loc init_col init_row in
  let new_loc = Utils.Location.apply_moves loc moves in
  assert_loc new_loc end_col end_row

let tests =
  "Test Suite for Utils.Location"
  >::: [
         ("on-board-uppercase" >:: fun _ -> basic_test 'F' 3);
         ("on-board-lowercase" >:: fun _ -> basic_test 'c' 7);
         ("off-board-column-large" >:: fun _ -> off_board_test 'J' 6);
         ("off-board-column-small" >:: fun _ -> off_board_test '9' 6);
         ("off-board-row-large" >:: fun _ -> off_board_test 'D' 13);
         ("off-board-row-small" >:: fun _ -> off_board_test 'E' (-2));
         ("move-up" >:: fun _ -> move_test 'A' 5 Utils.Move.Up 'A' 6);
         ("move-down" >:: fun _ -> move_test 'B' 8 Utils.Move.Down 'B' 7);
         ("move-left" >:: fun _ -> move_test 'G' 4 Utils.Move.Left 'F' 4);
         ("move-right" >:: fun _ -> move_test 'D' 2 Utils.Move.Right 'E' 2);
         ( "move-many-homogenous" >:: fun _ ->
           moves_test 'H' 6
             [ Utils.Move.Left; Utils.Move.Left; Utils.Move.Left ]
             'E' 6 );
         ( "move-many-heterogenous" >:: fun _ ->
           moves_test 'D' 8
             [ Utils.Move.Down; Utils.Move.Right; Utils.Move.Right ]
             'F' 7 );
       ]

let _ = run_test_tt_main tests
