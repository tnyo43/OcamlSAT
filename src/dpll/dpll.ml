open Sat

exception UnitPropagation of assign;;


let apply_assign cnf1 asgn =
  let (s, b) = asgn in
  let res = List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1 in
  if res = [] then raise Satisfied
  else res
;;

let unit_propagation cnf1 =
  try
    let _ = List.iter
              (fun cla -> 
                if List.length cla = 1
                then let lit = List.hd cla in raise (UnitPropagation (get_variable lit, get_state lit))
                else ())
              cnf1 in
    None
  with | UnitPropagation res -> Some res
;;

let next_assign_candidate cnf1 =
  match unit_propagation cnf1 with
  | Some (s, b) -> s, b, false
  | None -> let lit = List.hd @@ List.hd cnf1 in
        get_variable lit, get_state lit,  true
;;

(* 単位伝搬の時は同じレベルで計算をする
 * 決め打ちの時はレベルを一つあげて計算する
 * 矛盾が生じた時は *)
let rec solve_sub
      asgn_level_hist
      asgn_candidates
      cnf_hist
      next_asgn =
  let a, asgn, is_decision =
    match next_asgn with
    | None -> next_assign_candidate (List.hd cnf_hist)
    | Some (a, asgn) -> a, asgn, false
  in
  let new_candidate, new_cnf_hist, new_asgn_level_hist =
      if is_decision (* 決め打ちで決めたならレベルがひとつ上がる *)
      then (a, not asgn)::asgn_candidates, (List.hd cnf_hist)::cnf_hist, []::asgn_level_hist
      else asgn_candidates, cnf_hist, asgn_level_hist
  in
  let next_cnf_hist, next_asgn_level_hist, next_candidate, sat, next_asgn  =
    begin
      try
        let new_cnf : cnf = apply_assign (List.hd new_cnf_hist) (a, asgn) in
        let next_asgn_level_hist = ((a, asgn)::List.hd new_asgn_level_hist) :: List.tl new_asgn_level_hist in
        new_cnf::List.tl new_cnf_hist, next_asgn_level_hist, new_candidate, None, None
      with
      | Conflict -> begin
          match new_candidate with
          | [] -> raise Unsat (* 他の割り当てのしようがない *)
          | next_asgn::next_candidates ->
              List.tl cnf_hist,
              List.tl asgn_level_hist,
              next_candidates,
              None,
              Some next_asgn
        end
      | Satisfied -> [], [], [], Some((a, asgn)::List.flatten asgn_level_hist), None
    end in
  match sat with
  | Some res -> res
  | None -> solve_sub next_asgn_level_hist next_candidate next_cnf_hist next_asgn
;;

let solve cnf1 =
  let alph_set = make_alph_set cnf1 in
  let asgns = solve_sub [[]] [] [cnf1] None in
  let rests = SS.elements @@ List.fold_right (fun (s, _) -> SS.remove s) asgns alph_set in
  let result_assigns = List.fold_left (fun lst s -> (s, false)::lst) asgns rests in
  sort_assign result_assigns
;;