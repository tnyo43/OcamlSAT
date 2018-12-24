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

let apply_assign_test =
  "割り当てを適用した時の結果を得る。割り当てられない時はexceptionを返す" >::
    (fun _ ->
      assert_equal [[N "a"; P "b"]] (apply_assign [[N "a"; P "b"; N "c"]] ("c", true));
      assert_equal [] (apply_assign [[N "a"; P "b"; N "c"]] ("c", false));
      assert_equal [[P "a"; P "b"]] (apply_assign [[N "a"; N "b"; P "c"]; [P "a"; P "b"; N "c"]] ("c", true));
      assert_raises (Unsat) (fun _ -> apply_assign [[P "a"]] ("a", false));
    )
;;

let tests =
  "all_tests" >::: [
    update_clause_test;
    apply_assign_test;
  ]
;;