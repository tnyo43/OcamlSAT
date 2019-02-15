type assign = int * bool;;

type literal = int;;
let pos x = x;;
let neg x = -x;;

type clause = literal list;;
type cnf = clause list;;

type alphabets = int list;;

module IntOrd = struct
  type t = int
  let compare = Pervasives.compare
end
module SS = Set.Make(IntOrd);;

exception Satisfied;;
exception Unsat;;
exception Sat
exception UnitPropagation of assign;;

let default_assign = false;;

let get_variable lit = if lit > 0 then lit else -lit;;
let get_state lit = (lit > 0);;

let comp_int s1 s2 =
  if s1 = s2 then 0
  else if s1 > s2 then 1
  else -1
;;

let int_of_lit lit =
  let s = get_variable lit in
  let b = if get_state lit then 1 else 0 in
  s*2 + b
;;

let comp_literal lit1 lit2 =
  let v1 = int_of_lit lit1 in
  let v2 = int_of_lit lit2 in
  if v1 < v2 then -1
  else if v1 = v2 then 0
  else 1
;;

let comp_clause cla1 cla2 =
  let cla3 = List.sort comp_literal cla1 in
  let cla4 = List.sort comp_literal cla2 in
  let rec comp_clause cla1 cla2 =
    match cla1, cla2 with
    | h1::t1, h2::t2 ->
        let res = comp_literal h1 h2 in
        if res = 0 then comp_clause t1 t2 else res
    | _::_, [] -> 1
    | [], _::_ -> -1
    | [], [] -> 0
  in comp_clause cla3 cla4
;;

let comp_cnf cnf1 cnf2 =
  let cnf3 = List.sort comp_clause cnf1 in
  let cnf4 = List.sort comp_clause cnf2 in
  let rec comp_cnf cnf1 cnf2 =
    match cnf1, cnf2 with
    | h1::t1, h2::t2 ->
        let res = comp_clause h1 h2 in
        if res = 0 then comp_cnf t1 t2 else res
    | _::_, [] -> 1
    | [], _::_ -> -1
    | [], [] -> 0
  in comp_cnf cnf3 cnf4
;;

let is_different_each_other comp lst =
  let sorted_list = List.sort comp lst in
  let rec check lst =
    match lst with
    | x::y::t -> if comp x y = 0 then false else check (y::t)
    | _ -> true
  in
  check sorted_list
;;


let make_alph_set cnf1 =
  let add_set cla set = List.fold_right (fun lit -> SS.add @@ get_variable lit) cla set in
  List.fold_right (fun cla -> add_set cla) cnf1 SS.empty
;;

let sort_assign asgns =
  List.sort (fun asgn1 asgn2 ->
    match (asgn1, asgn2) with
    | (x, _), (y, _) -> if x > y then 1 else if x = y then 0 else (-1)) asgns
;;

let update_clause cla s b =
  try
    if List.length cla = 1
    then
      let lit = List.hd cla in
      if get_variable lit = s
      then
        if get_state lit = b then raise Satisfied else raise Unsat
      else cla
    else List.fold_right
            (fun lit res ->
                if get_variable lit = s
                then
                  if get_state lit = b then raise Satisfied else res
                else lit :: res)
            cla []
  with | Satisfied -> []
;;

let apply_assign cnf1 asgn =
  let (s, b) = asgn in
  let res = List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1 in
  if res = [] then raise Sat
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
      | Unsat -> begin
          match new_candidate with
          | [] -> raise Unsat (* 他の割り当てのしようがない *)
          | next_asgn::next_candidates ->
              List.tl cnf_hist,
              List.tl asgn_level_hist,
              next_candidates,
              None,
              Some next_asgn
        end
      | Sat -> [], [], [], Some((a, asgn)::List.flatten asgn_level_hist), None
    end in
  match sat with
  | Some res -> res
  | None -> solve_sub next_asgn_level_hist next_candidate next_cnf_hist next_asgn
;;

let solve cnf1 =
  let alph_set = make_alph_set cnf1 in
  let asgns = solve_sub [[]] [] [cnf1] None in
  let rests = SS.elements @@ List.fold_right (fun (s, _) -> SS.remove s) asgns alph_set in
  let result_assigns = List.fold_left (fun lst s -> (s, default_assign)::lst) asgns rests in
  sort_assign result_assigns
;;

let checker cnf1 asgns =
  let checker_sub cla asgns =
    try
      let res = List.fold_left (fun cla (s,b) -> update_clause cla s b) cla asgns in
      res = []
    with
    | Unsat -> false
  in
  List.fold_left (fun res cla -> res && checker_sub cla asgns) true cnf1
;;