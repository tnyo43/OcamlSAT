open OUnit2
open Sudoku

let cell_and_num_to_alphabet_test =
  "セルの位置と値から割り当てのキーを作成する" >::
    (fun _ ->
      assert_equal "abc" (cell_and_num_to_alphabet 1 2 3);
      assert_equal "kpu" (cell_and_num_to_alphabet 11 16 21);
    )
;;

let init_line_test =
  "横一列の割り当てを全て作成する" >::
    (fun _ ->
      assert_equal 96 (List.length @@ List.filter (fun x -> List.length x = 2) @@ init_line 4);
      assert_equal 16 (List.length @@ List.filter (fun x -> List.length x = 4) @@ init_line 4);
      assert_equal (96+16) (List.length @@ init_line 4);
    )
;;

let tests =
  "all_tests" >::: [
    cell_and_num_to_alphabet_test;
    init_line_test;
  ]
;;