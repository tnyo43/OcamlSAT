open OUnit2
open Dpll
open Cdcl

let and_clause_test =
  "二つの項の積を取る" >::
    (fun _ ->
      assert_equal [pos 1; pos 2; neg 3; neg 4] (and_clause [pos 1; pos 2] [neg 3; neg 4]);
      assert_equal [pos 1; neg 2; pos 3; neg 4] (and_clause [pos 1; neg 2; pos 3] [pos 3; neg 4]);
      assert_equal [pos 1; pos 3; neg 4] (and_clause [pos 1; neg 2; pos 3] [pos 2; pos 3; neg 4]);
      assert_raises Unsat (fun _ -> and_clause [pos 1] [neg 1]);
    )
;;

let update_cdcl_clause_test = 
  "CDCLのタイプの項を更新する。矛盾が生じたときは新たな学習項を生成する" >::
    (fun _ ->
      assert_equal ([], [pos 1; pos 2]) (update_cdcl_clause ([pos 1; pos 2], [pos 1; pos 2]) [pos 1; neg 3] 1 true);
      assert_equal ([pos 2], [pos 1; pos 2]) (update_cdcl_clause ([pos 1; pos 2], [pos 1; pos 2]) [neg 1; neg 3] 1 false);
      assert_raises (NewClause [pos 2; neg 3]) (fun _ -> update_cdcl_clause ([pos 1], [pos 1; pos 2]) [neg 1; neg 3] 1 false);
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
  "CNFから次の割り当てを決定。単位伝播があるならそれを、ないなら適当に真を当てる" >::
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
        (diagnose 
          [[neg 2; neg 7; pos 9]; [neg 7; pos 8]; [neg 5; neg 6; pos 7]; [neg 4; pos 6]; [neg 1; neg 4; pos 5]]
          [(4, true); (5, true); (6, true); (7, true); (8, true); (9, true)]
          [neg 8; neg 9]
        );
      assert_equal
        [neg 1; neg 4; pos 7]
        (diagnose
          [[neg 4; pos 6]; [neg 1; neg 4; pos 5]]
          [(4, true); (5, true); (6, true)]
          [neg 5; neg 6; pos 7]
        );
      assert_equal
        [neg 8]
        (diagnose
          [[neg 8; neg 9]; [neg 5; neg 6; pos 7]; [neg 1; neg 4; pos 7]; [neg 2; neg 7]]
          [(8, true); (9, false)]
          [neg 8; pos 9]
        );
    )

let check_level_test =
  "項に登場する変数のうち、2番目に登場が遅いレベル" >::
    (fun _ ->
      assert_equal 2 (check_level [neg 2; neg 7] [[(4, true); (5, true); (6, true); (7, true); (8, true); (9, true)]; [(3, true)]; [(2, true)]; [(1, true)]]);
      assert_equal 2 (check_level [neg 1; neg 4; pos 7] [[(4, true); (5, true); (6, true)]; [(3, true)]; [(2, true); (7, true)]; [(1, true)]]);
      assert_equal 0 (check_level [neg 8] [[(8, true); (9, true)]; [(5, true); (6, false)]; [(3, true)]; [(2, true); (7, false); (4, false)]; [(1, true)]])
    )
;;

let tests =
  "all_tests" >::: [
    and_clause_test;
    update_cdcl_clause_test;
    unit_propagation_test;
    next_assign_test;
    diagnose_test;
    check_level_test;
  ]
;;