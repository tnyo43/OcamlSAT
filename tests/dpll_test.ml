open OUnit2
open Dpll

let update_clause_test =
  "新しい割り当てが項に含まれていれば更新、なければ何もしない" >::
    (fun _ ->
      assert_equal [N "a"; P "b"; N "c"] (update_clause [N "a"; P "b"; N "c"; P "d"] "d" false); 
      assert_equal [] (update_clause [N "a"; P "b"; N "c"; P "d"] "b" true); 
      assert_equal [N "a"; P "b"; P "d"] (update_clause [N "a"; P "b"; N "c"; P "d"] "c" true); 
      assert_equal [N "a"; P "b"; N "c"; P "d"] (update_clause [N "a"; P "b"; N "c"; P "d"] "f" false); 
    )
;;

let tests =
  "all_tests" >::: [
    update_clause_test;
  ]
;;