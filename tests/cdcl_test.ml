open OUnit2
open Dpll
open Cdcl

let and_clause_test =
  "二つの項の積を取る" >::
    (fun _ ->
      assert_equal [P 1; P 2; N 3; N 4] (and_clause [P 1; P 2] [N 3; N 4]);
      assert_equal [P 1; N 2; P 3; N 4] (and_clause [P 1; N 2; P 3] [P 3; N 4]);
      assert_equal [P 1; P 3; N 4] (and_clause [P 1; N 2; P 3] [P 2; P 3; N 4]);
      assert_raises Unsat (fun _ -> and_clause [P 1] [N 1]);
    )
;;

let update_cdcl_clause_test = 
  "CDCLのタイプの項を更新する。矛盾が生じたときは新たな学習項を生成する" >::
    (fun _ ->
      assert_equal ([], [P 1; P 2]) (update_cdcl_clause ([P 1; P 2], [P 1; P 2]) [P 1; N 3] 1 true);
      assert_equal ([P 2], [P 1; P 2]) (update_cdcl_clause ([P 1; P 2], [P 1; P 2]) [N 1; N 3] 1 false);
      assert_raises (NewClause [P 2; N 3]) (fun _ -> update_cdcl_clause ([P 1], [P 1; P 2]) [N 1; N 3] 1 false);
    )
;;

let unit_propagation_test =
  "単位伝播。更新したほうの項をみて、単位伝播がないときはNoneを返す" >::
    (fun _ ->
      assert_equal None (unit_propagation [([P 1; P 2], [P 1; P 2]); ([N 1; N 2], [N 1; N 2])]);
      assert_equal (Some (1, true)) (unit_propagation [([P 1], [P 1; P 2]); ([N 1; N 2], [N 1; N 2])]);
      assert_equal None (unit_propagation [([N 1; P 2], [N 1; P 2]); ([N 1; N 2], [N 1; N 2])]);
    )
;;

let tests =
  "all_tests" >::: [
    and_clause_test;
    update_cdcl_clause_test;
    unit_propagation_test;
  ]
;;