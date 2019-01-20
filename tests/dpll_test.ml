open OUnit2
open Dpll

let update_clause_test =
  "新しい割り当てが項に含まれていれば更新、なければ何もしない" >::
    (fun _ ->
      assert_equal [N 1; P 2; N 3] (update_clause [N 1; P 2; N 3; P 4] 4 false); 
      assert_equal [] (update_clause [N 1; P 2; N 3; P 4] 2 true); 
      assert_equal [N 1; P 2; P 4] (update_clause [N 1; P 2; N 3; P 4] 3 true); 
      assert_equal [N 1; P 2; N 3; P 4] (update_clause [N 1; P 2; N 3; P 4] 6 false); 
    )
;;

let apply_assign_test =
  "割り当てを適用した時の結果を得る。割り当てられない時はexceptionを返す。充足できれば充足する割り当てを返す" >::
    (fun _ ->
      assert_equal [[N 1; P 2]] (apply_assign [[N 1; P 2; N 3]] (3, true) []);
      assert_raises (Sat [(3, false); (4, false); (5, true)]) (fun _ -> apply_assign [[N 1; P 2; N 3]] (3, false) [(4, false); (5, true)]);
      assert_equal [[P 1; P 2]] (apply_assign [[N 1; N 2; P 3]; [P 1; P 2; N 3]] (3, true) []);
      assert_raises (Unsat) (fun _ -> apply_assign [[P 1]] (1, false) []);
    )
;;

let unit_propagation_test =
  "単位伝播のテスト。単位伝播がないときはNoneを返す" >::
    (fun _ ->
      assert_equal None (unit_propagation [[P 1; P 2]; [N 1; N 2]]);
      assert_equal (Some (1, true)) (unit_propagation [[P 1]; [N 1; N 2]]);
      assert_equal None (unit_propagation [[N 1; P 2]; [N 1; N 2]]);
    )

let next_assign_list_test =
  "単位伝播がある時はそれを次の割り当てとする。そうでない時は最初のアルファベットとする" >::
    (fun _ ->
      assert_equal (2, [(2, false)]) (next_assign_list [[N 2]; [N 1; P 2]] [1; 2]);
      assert_equal (1, [(1, false); (1, true)]) (next_assign_list [[P 1; N 2]; [N 1; P 2]] [1; 2]);
    )
;;

let solve_sat_test =
  "SATのテスト。成功すると割り当てのリストを返し、失敗するとUnsatになる" >::
    (fun _ ->
      assert_equal [(1, false); (2, true); (3, true); (4, false)] (solve [[P 1; P 2]; [N 1; N 2]; [P 3; P 4]; [P 3; N 4]]);
      assert_equal
        [(1, true); (2, true); (3, true); (4, true)]
        (solve [
          [N 1; P 2; P 3];
          [P 1; P 3; P 4];
          [P 1; P 3; N 4];
          [P 1; N 3; P 4];
          [P 1; N 3; N 4];
          [N 2; N 3; P 4];
          [N 1; P 2; N 3];
          [N 1; N 2; P 3];
          [P 1; P 3]
        ]);
      assert_raises (Unsat) (fun _ -> solve [[N 1]; [P 1]])
    )
;;

let tests =
  "all_tests" >::: [
    update_clause_test;
    apply_assign_test;
    unit_propagation_test;
    next_assign_list_test;
    solve_sat_test;
  ]
;;