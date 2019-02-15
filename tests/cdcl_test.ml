open OUnit2
open Sat
open Cdcl

let and_clause_tester cla1 cla2 ignore_list =
  let new_clause, new_ignore_list = and_clause cla1 cla2 ignore_list in
  List.sort comp_literal new_clause, List.sort comp_literal new_ignore_list
;;

let and_clause_test =
  "二つの項の積を取る" >::
    (fun _ ->
      assert_equal ([pos 1; pos 2; neg 3; neg 4], []) (and_clause_tester [pos 1; pos 2] [neg 3; neg 4] []);
      assert_equal ([pos 1; neg 2; pos 3; neg 4], []) (and_clause_tester [pos 1; neg 2; pos 3] [pos 3; neg 4] []);
      assert_equal ([pos 1; pos 3; neg 4], [2]) (and_clause_tester [pos 1; neg 2; pos 3] [pos 2; pos 3; neg 4] []);
      assert_equal ([pos 1], [2;3;4]) (and_clause_tester [pos 1] [pos 2; pos 3; neg 4] [2;3;4]);
      assert_raises Conflict (fun _ -> and_clause_tester [pos 1] [neg 1] []);
    )
;;

let unit_propagation_test =
  "単位伝播。単位伝播がないときはNoneを返す。あるときは伝播した結果とそれの元となった項を返す" >::
    (fun _ ->
      assert_equal None (unit_propagation [([pos 1; pos 2], [pos 1; pos 2]); ([neg 1; neg 2], [neg 1; neg 2])]);
      assert_equal (Some (pos 1, [pos 1; pos 2])) (unit_propagation [([pos 1], [pos 1; pos 2]); ([neg 1; neg 2], [neg 1; neg 2])]);
      assert_equal None (unit_propagation [([neg 1; pos 2], [neg 1; pos 2]); ([neg 1; neg 2], [neg 1; neg 2])]);
    )
;;

let next_assign_test =
  "CNFから次の割り当てを決定。単位伝播があるならそれと割り当ての根拠になった項、ないなら適当に真を当てる" >::
    (fun _ ->
      assert_equal ((2, false), Some [neg 2; pos 3]) (next_assign [([neg 2], [neg 2; pos 3]); ([neg 1; pos 2],[neg 1; pos 2])] [1; 2]);
      assert_equal ((1, true), None) (next_assign [([pos 1; neg 2],[pos 1; neg 2]); ([neg 1; pos 2],[neg 1; pos 2])] [1; 2]);
    )
;;

let diagnose_test =
  "新しい項を作る" >::
    (fun _ ->
      assert_equal
        [neg 2; neg 7]
        (List.sort comp_literal @@ diagnose 
          [[neg 2; neg 7; pos 9]; [neg 5; neg 6; pos 7]; [neg 4; pos 6]; [neg 1; neg 4; pos 5]]
          [(9, true); (4, true); (5, true); (6, true); (7, true)]
          [neg 8; neg 9] [neg 7; pos 8]
        );
      assert_equal
        [neg 1; neg 4; pos 7]
        (List.sort comp_literal @@ diagnose
          [[neg 1; neg 4; pos 5]]
          [(5, true); (4, true)]
          [neg 5; neg 6; pos 7] [neg 4; pos 6]
        );
      assert_equal
        [neg 8]
        (List.sort comp_literal @@ diagnose
          [[neg 5; neg 6; pos 7]; [neg 1; neg 4; pos 7]; [neg 2; neg 7]]
          [(8, true); (9, false)]
          [neg 8; pos 9] [neg 8; neg 9]
        );
    )
;;

let check_level_test =
  "項に登場する変数のうち、2番目に登場が遅いレベル" >::
    (fun _ ->
      assert_equal 2 (check_level [neg 2; neg 7] [[(4, true); (5, true); (6, true); (7, true); (8, true); (9, true)]; [(3, true)]; [(2, true)]; [(1, true)]]);
      assert_equal 2 (check_level [neg 1; neg 4; pos 7] [[(4, true); (5, true); (6, true)]; [(3, true)]; [(2, true); (7, true)]; [(1, true)]]);
      assert_equal 0 (check_level [neg 8] [[(8, true); (9, true)]; [(5, true); (6, false)]; [(3, true)]; [(2, true); (7, false); (4, false)]; [(1, true)]])
    )
;;

let solve_sub_test =
  "solveのサブ関数のテスト" >::
    (fun _ ->
      assert_equal [(1, true)] (solve_sub [[]] [[([pos 1], [pos 1])]] [[]] [[pos 1]] [1] [1]);
      assert_equal [(1, true); (2, false)]
      (solve_sub [[]]
        [[
          ([pos 1; neg 2], [pos 1; neg 2]);
          ([neg 1; neg 2], [neg 1; neg 2])
        ]][[]] [[pos 1; neg 2]; [neg 1; neg 2]] [1; 2] [1; 2]
      );
      assert_raises Unsat
      (fun _ -> solve_sub [[]]
        [[
          ([pos 1], [pos 1]);
          ([neg 1], [neg 1])
        ]] [[]] [[pos 1]; [neg 1]] [1] [1]
      );
    )
;;

let solve_test =
  "最終テスト" >::
  (fun _ ->
    assert_equal [(1, true)] (solve [[pos 1]]);
    assert_equal [(1, true); (2, false)] (solve [[pos 1; neg 2]; [neg 1; neg 2]]);
    assert_raises Unsat (fun _ -> solve [[pos 1]; [neg 1]]);
    assert_equal [(1, true); (2, true); (3, true); (4, true)]
        (solve [
          [neg 1; pos 2; pos 3];
          [pos 1; pos 3; pos 4];
          [pos 1; pos 3; neg 4];
          [pos 1; neg 3; pos 4];
          [pos 1; neg 3; neg 4];
          [neg 2; neg 3; pos 4];
          [neg 1; pos 2; neg 3];
          [neg 1; neg 2; pos 3];
          [pos 1; pos 3]
        ]);
    assert_equal [(1, true); (2, true); (4, false); (5, true); (6, false); (7, false); (8, false); (9, false)]
        (solve [
          [neg 1; neg 4; pos 5];
          [neg 4; pos 6];
          [neg 5; neg 6; pos 7];
          [neg 7; pos 8];
          [neg 2; neg 7; pos 9];
          [neg 8; neg 9];
          [neg 8; pos 9]
        ])
  )
;;

let tests =
  "all_tests" >::: [
    and_clause_test;
    unit_propagation_test;
    next_assign_test;
    diagnose_test;
    check_level_test;
    solve_sub_test;
    solve_test;
  ]
;;