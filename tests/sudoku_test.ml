open OUnit2
open Dpll
open Sudoku

let alphabet_of_cell_and_num_test =
  "セルの位置と値から割り当てのキーを作成する" >::
    (fun _ ->
      assert_equal 6 (variable_of_cell_and_num 4 1 2 3);
      assert_equal 156645 (variable_of_cell_and_num 25 11 16 21);
    )
;;

let sudoku_cnf = init_sudoku 4;; 

let init_part_test =
  "セルごと、縦、横一列、ブロックごとの割り当てを全て作成する。重複はない" >::
    (fun _ ->
      assert_equal true (is_different_each_other comp_clause sudoku_cnf);
      assert_equal true (List.mem
                          [pos (variable_of_cell_and_num 4 1 1 1)
                          ;pos (variable_of_cell_and_num 4 1 1 2)
                          ;pos (variable_of_cell_and_num 4 1 1 3)
                          ;pos (variable_of_cell_and_num 4 1 1 4)]
                          sudoku_cnf);
      assert_equal true (List.mem 
                          [neg (variable_of_cell_and_num 4 1 1 3)
                          ;neg (variable_of_cell_and_num 4 1 2 3)]
                          sudoku_cnf);
      assert_equal true (List.mem 
                          [neg (variable_of_cell_and_num 4 1 1 4)
                          ;neg (variable_of_cell_and_num 4 3 1 4)]
                          sudoku_cnf);
      assert_equal true (List.mem 
                          [neg (variable_of_cell_and_num 4 1 1 1)
                          ;neg (variable_of_cell_and_num 4 2 2 1)]
                          sudoku_cnf);
      assert_equal false (List.mem 
                          [neg (variable_of_cell_and_num 4 1 1 1)
                          ;neg (variable_of_cell_and_num 4 3 3 1)]
                          sudoku_cnf);
    )
;;

let problem1 = [
  [0;0;0;4];
  [0;0;1;2];
  [0;0;4;3];
  [4;3;2;1]
];;
let ans1 = [
  [2;1;3;4];
  [3;4;1;2];
  [1;2;4;3];
  [4;3;2;1]
];;
let problem2 = [
  [0;0;8;0;0;0;0;0;4];
  [3;7;0;0;0;2;1;0;0];
  [0;2;0;0;0;0;7;5;0];
  [0;5;1;0;0;0;8;9;6];
  [2;0;0;0;7;0;0;1;0];
  [0;4;9;0;6;1;2;3;0];
  [4;3;2;6;1;8;5;0;9];
  [0;8;6;7;9;5;0;4;0];
  [5;9;7;4;0;3;6;8;0]
];;
let ans2 = [
  [6;1;8;3;5;7;9;2;4];
  [3;7;5;9;4;2;1;6;8];
  [9;2;4;1;8;6;7;5;3];
  [7;5;1;2;3;4;8;9;6];
  [2;6;3;8;7;9;4;1;5];
  [8;4;9;5;6;1;2;3;7];
  [4;3;2;6;1;8;5;7;9];
  [1;8;6;7;9;5;3;4;2];
  [5;9;7;4;2;3;6;8;1]
]
let problem3 = [
  [1;0;0;0];
  [2;0;0;0];
  [3;0;0;0];
  [0;4;3;2]
];;

let solve_sudoku_test =
  "数独の問題を解く。解がない問題はエラー" >::
    (fun _ ->
      assert_equal ans1 (solve_sudoku problem1 4);
      assert_equal ans2 (solve_sudoku problem2 9);
      assert_raises Unsat (fun _ -> solve_sudoku problem3 4);
    )
;;

let tests =
  "all_tests" >::: [
    (*
    alphabet_of_cell_and_num_test;
    init_part_test;
    *)
    solve_sudoku_test;
  ]
;;