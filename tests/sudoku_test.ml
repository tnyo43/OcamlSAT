open OUnit2
open Dpll
open Sudoku

let alphabet_of_cell_and_num_test =
  "セルの位置と値から割り当てのキーを作成する" >::
    (fun _ ->
      assert_equal "abc" (alphabet_of_cell_and_num 1 2 3);
      assert_equal "kpu" (alphabet_of_cell_and_num 11 16 21);
    )
;;

let sudoku_cnf = init_for_cell 4 @@ init_line 4 @@ init_column 4 @@ init_block 4 [] 

let init_part_test =
  "セルごと、縦、横一列、ブロックごとの割り当てを全て作成する。重複はない" >::
    (fun _ ->
      assert_equal true (is_different_each_other comp_clause sudoku_cnf);
      assert_equal true (List.mem [P "aaa"; P "aab"; P "aac"; P "aad"] sudoku_cnf);
      assert_equal true (List.mem [N "aac"; N "abc"] sudoku_cnf);
      assert_equal true (List.mem [N "aad"; N "cad"] sudoku_cnf);
      assert_equal true (List.mem [N "aaa"; N "bba"] sudoku_cnf);
      assert_equal false (List.mem [N "aaa"; N "cca"] sudoku_cnf);
    )
;;

let tests =
  "all_tests" >::: [
    alphabet_of_cell_and_num_test;
    init_part_test;
  ]
;;