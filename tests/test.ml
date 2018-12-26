open OUnit2

let all_tests = "all_tests" >::: [
  Dpll_test.tests;
  Sudoku_test.tests;
]

let () =
  run_test_tt_main all_tests;;
