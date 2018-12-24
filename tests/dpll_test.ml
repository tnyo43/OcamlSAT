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
  "割り当てを適用した時の結果を得る。割り当てられない時はexceptionを返す。充足できれば充足する割り当てを返す" >::
    (fun _ ->
      assert_equal [[N "a"; P "b"]] (apply_assign [[N "a"; P "b"; N "c"]] ("c", true) []);
      assert_raises (Sat [("c", false); ("d", false); ("e", true)]) (fun _ -> apply_assign [[N "a"; P "b"; N "c"]] ("c", false) [("d", false); ("e", true)]);
      assert_equal [[P "a"; P "b"]] (apply_assign [[N "a"; N "b"; P "c"]; [P "a"; P "b"; N "c"]] ("c", true) []);
      assert_raises (Unsat) (fun _ -> apply_assign [[P "a"]] ("a", false) []);
    )
;;

let next_assign_list_test =
  "単位伝播がある時はそれを次の割り当てとする。そうでない時は最初のアルファベットとする" >::
    (fun _ ->
      assert_equal [("b", false)] (next_assign_list [[N "b"]; [N "a"; P "b"]] ["a"; "b"]);
      assert_equal [("a", false); ("a", true)] (next_assign_list [[P "a"; N "b"]; [N "a"; P "b"]] ["a"; "b"]);
    )
;;

let tests =
  "all_tests" >::: [
    update_clause_test;
    apply_assign_test;
    next_assign_list_test;
  ]
;;