open OUnit2
open Dpll

let update_clause_test =
  "新しい割り当てが項に含まれていれば更新、なければ何もしない" >::
    (fun _ ->
      assert_equal [neg 1; pos 2; neg 3] (update_clause [neg 1; pos 2; neg 3; pos 4] 4 false); 
      assert_equal [] (update_clause [neg 1; pos 2; neg 3; pos 4] 2 true); 
      assert_equal [neg 1; pos 2; pos 4] (update_clause [neg 1; pos 2; neg 3; pos 4] 3 true); 
      assert_equal [neg 1; pos 2; neg 3; pos 4] (update_clause [neg 1; pos 2; neg 3; pos 4] 6 false); 
    )
;;

let apply_assign_test =
  "割り当てを適用した時の結果を得る。割り当てられない時はexceptionを返す。充足できれば充足する割り当てを返す" >::
    (fun _ ->
      assert_equal [[neg 1; pos 2]] (apply_assign [[neg 1; pos 2; neg 3]] (3, true));
      assert_raises Satisfied (fun _ -> apply_assign [[neg 1; pos 2; neg 3]] (3, false));
      assert_equal [[pos 1; pos 2]] (apply_assign [[neg 1; neg 2; pos 3]; [pos 1; pos 2; neg 3]] (3, true));
      assert_raises Unsat (fun _ -> apply_assign [[pos 1]] (1, false));
    )
;;

let unit_propagation_test =
  "単位伝播のテスト。単位伝播がないときはNoneを返す" >::
    (fun _ ->
      assert_equal None (unit_propagation [[pos 1; pos 2]; [neg 1; neg 2]]);
      assert_equal (Some (1, true)) (unit_propagation [[pos 1]; [neg 1; neg 2]]);
      assert_equal None (unit_propagation [[neg 1; pos 2]; [neg 1; neg 2]]);
    )

let next_assign_candidate_test =
  "次の割り当てが単位伝搬なら、それとfalseを返す。割り当てなら'変数','仮の割り当て(cnfの最初の項を満たすような)', 'true'を返す" >::
    (fun _ ->
      assert_equal (2, false, false) (next_assign_candidate [[neg 2]; [neg 1; pos 2]]);
      assert_equal (1, true, true) (next_assign_candidate [[pos 1; neg 2]; [neg 1; pos 2]]);
      assert_equal (3, true, false) (next_assign_candidate [[pos 3]; [neg 1; pos 2; neg 3]]);
    )
;;

let solve_sat_test =
  "SATのテスト。成功すると割り当てのリストを返し、失敗するとUnsatになる" >::
    (fun _ ->
      let cnf1 = [[pos 1; pos 2]; [neg 1; neg 2]; [pos 3; pos 4]; [pos 3; neg 4]] in
      assert_equal true (checker cnf1 @@ solve cnf1);
      let cnf2 = [
          [neg 1; pos 2; pos 3];
          [pos 1; pos 3; pos 4];
          [pos 1; pos 3; neg 4];
          [pos 1; neg 3; pos 4];
          [pos 1; neg 3; neg 4];
          [neg 2; neg 3; pos 4];
          [neg 1; pos 2; neg 3];
          [neg 1; neg 2; pos 3];
          [pos 1; pos 3]
        ] in
      assert_equal true (checker cnf2 @@ solve cnf2);
      assert_raises (Unsat) (fun _ -> solve [[neg 1]; [pos 1]])
    )
;;

let checker_test =
  "回答がSATを満たすかどうか判定、UNSATISFYの判定はこれではしない" >::
    (fun _ ->
      assert_equal true (checker [
          [neg 1; pos 2; pos 3];
          [pos 1; pos 3; pos 4];
          [pos 1; pos 3; neg 4];
          [pos 1; neg 3; pos 4];
          [pos 1; neg 3; neg 4];
          [neg 2; neg 3; pos 4];
          [neg 1; pos 2; neg 3];
          [neg 1; neg 2; pos 3];
          [pos 1; pos 3]
        ] [(1, true); (2, true); (3, true); (4, true)]);
      assert_equal false (checker [
          [neg 1; pos 2; pos 3];
          [pos 1; pos 3; pos 4];
          [pos 1; pos 3; neg 4];
          [pos 1; neg 3; pos 4];
          [pos 1; neg 3; neg 4];
          [neg 2; neg 3; pos 4];
          [neg 1; pos 2; neg 3];
          [neg 1; neg 2; pos 3];
          [pos 1; pos 3]
        ] [(1, false); (2, true); (3, true); (4, true)]);
      assert_equal true (checker [
          [neg 1; neg 4; pos 5];
          [neg 4; pos 6];
          [neg 5; neg 6; pos 7];
          [neg 7; pos 8];
          [neg 2; neg 7; pos 9];
          [neg 8; neg 9];
          [neg 8; pos 9]
        ] [(1, true); (2, true); (4, false); (5, true); (6, false); (7, false); (8, false); (9, false)]);
      assert_equal true (checker [
          [neg 1; neg 4; pos 5];
          [neg 4; pos 6];
          [neg 5; neg 6; pos 7];
          [neg 7; pos 8];
          [neg 2; neg 7; pos 9];
          [neg 8; neg 9];
          [neg 8; pos 9]
        ] [(1, true); (2, true); (4, false); (5, true); (6, false); (7, false); (8, false); (9, true)]);
    )
;;

let tests =
  "all_tests" >::: [
    update_clause_test;
    apply_assign_test;
    unit_propagation_test;
    next_assign_candidate_test;
    checker_test;
    solve_sat_test;
  ]
;;