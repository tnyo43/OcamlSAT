open Dpll

type cdcl_clause = clause * clause;;
type cdcl_cnf = cdcl_clause list;;

exception NewClause of clause

let create_cdcl_cnf cnf1 =
  List.fold_right (fun cla cdcl_cnf1 -> (cla, cla)::cdcl_cnf1) cnf1 []
;;

let and_clause cla1 cla2 =
  let lit_list = List.sort comp_literal (cla1@cla2) in
  let rec union lst k =
    match lst with
    | [] -> k []
    | h::[] -> k lst
    | h1::h2::t ->
        if get_variable h1 = get_variable h2
        then
          if get_state h1 = get_state h2
          then union (h2::t) k
          else union t k
        else union (h2::t) (fun x -> k (h1::x))
  in
  let res = union lit_list (fun x -> x) in
  if List.length res = 0 then raise Unsat else res
;;

let update_cdcl_clause cdcl_cla cla s b =
  let current_cla, original_cla = cdcl_cla in
  try
    (update_clause current_cla s b, original_cla)
  with
  | Unsat -> raise (NewClause (and_clause original_cla cla))
;;

let apply_assign cnf1 asgn asgns =
  let (s, b) = asgn in
  let res = List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1 in
  if res = [] then raise (Sat (asgn :: asgns))
  else res
;;

let unit_propagation cnf1 =
  try
    let _ = List.iter
              (fun (current_cla, _) ->
                if List.length current_cla = 1
                then let lit = List.hd current_cla in raise (UnitPropagation (get_variable lit, get_state lit))
                else ())
              cnf1 in
    None
  with | UnitPropagation res -> Some res
;;

let next_assign_list cnf1 alphs =
  match unit_propagation cnf1 with
  | Some (s, b) -> (s, [(s, b)])
  | None -> (List.hd alphs, [(List.hd alphs, false); (List.hd alphs, true)])
;;
(*
let solve cnf1 =
  let rec solve cnf1 alphs asgns =
    let (a, next_assigns) = next_assign_list cnf1 alphs in
    let next_alphs = List.filter (fun x -> not(x = a)) alphs in
    let rec try_asgn next_assigns =
      match next_assigns with
      | [] -> raise Unsat
      | asgn::tl ->
          try
            let cnf2 = apply_assign cnf1 asgn asgns in
            let _ = solve cnf2 next_alphs (asgn::asgns) in []
          with Unsat -> try_asgn tl
    in
    try_asgn next_assigns
  in
  let alph_set = make_alph_set cnf1 in
  let alphs = SS.elements alph_set in
  try let _ = solve cnf1 alphs [] in raise Unsat
  with Sat asgns -> 
    let rests = SS.elements @@ List.fold_right (fun (s, _) -> SS.remove s) asgns alph_set in
    let result_assigns = List.fold_left (fun lst s -> (s, default_assign)::lst) asgns rests in
    sort_assign result_assigns
;;
*)