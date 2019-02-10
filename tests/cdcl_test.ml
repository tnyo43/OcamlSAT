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
  "単位伝播。更新したほうの項をみて、単位伝播がないときはNoneを返す" >::
    (fun _ ->
      assert_equal None (unit_propagation [([pos 1; pos 2], [pos 1; pos 2]); ([neg 1; neg 2], [neg 1; neg 2])]);
      assert_equal (Some (1, true)) (unit_propagation [([pos 1], [pos 1; pos 2]); ([neg 1; neg 2], [neg 1; neg 2])]);
      assert_equal None (unit_propagation [([neg 1; pos 2], [neg 1; pos 2]); ([neg 1; neg 2], [neg 1; neg 2])]);
    )
;;

let tests =
  "all_tests" >::: [
    and_clause_test;
    update_cdcl_clause_test;
    unit_propagation_test;
  ]
;;