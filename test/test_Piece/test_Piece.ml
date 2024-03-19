open OUnit2

let tests = "example test suite" >::: [
  "example" >:: (fun _ -> assert_equal 0 0);
]

let _ = run_test_tt_main tests