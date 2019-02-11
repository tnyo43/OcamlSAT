open Dpll

type deduce = literal * clause * int (* 変数・原因になった項・レベル *)
type cdcl_clause = clause * clause;;
type cdcl_cnf = cdcl_clause list;;

module LevelMap = Map.Make(IntOrd);;

exception CdclUnitPropagation of (literal * clause);;
exception NewClause of clause;;
exception CdclSat of (assign list) list;;
exception CdclUnsat of (int * clause);;

let create_cdcl_cnf cnf1 =
  List.fold_right (fun cla cdcl_cnf1 -> (cla, cla)::cdcl_cnf1) cnf1 []
;;

let cdcl_cnf_of_cnf cnf1 =
  List.map (fun cla -> (cla, cla)) cnf1
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
  let res = List.filter (fun (cla, _) -> List.length cla > 0) 
         @@ List.map (fun (cla, original_cla) -> update_clause cla s b, original_cla) cnf1 in
  if res = [] then raise (CdclSat ([asgn] :: asgns))
  else res
;;

(* 単位伝播があるとき、その伝播をもたらした元々の項を教える *)
let unit_propagation cnf1 =
  try
    let _ = List.iter
              (fun (current_cla, original_cla) ->
                if List.length current_cla = 1
                then let lit = List.hd current_cla in raise (CdclUnitPropagation (lit, original_cla))
                else ())
              cnf1 in
    None
  with | CdclUnitPropagation res -> Some res
;;

let next_assign cnf1 alphs =
  match unit_propagation cnf1 with
  | Some (lit, cla) -> let a = get_variable lit in (a, get_state lit), Some cla
  | None -> let a = List.hd alphs in (a, true), None
;;

let update_level_asgns asgns asgn =
  match asgns with
  | hd::tl -> (asgn::hd) :: tl
;;

(* 最大レベルの変数が出て来ていいのは1回まで、 *)
let rec is_valid_clause cla level_asgns count =
  match level_asgns with
  | [] -> count <= 1
  | (s, b)::tl -> is_valid_clause cla tl @@
      List.fold_left (fun count lit -> if get_variable lit = s then count+1 else count) count cla
;;

(* 新しい項を作る *)
let rec diagnose cla_hist level_asgns cla =
  if (is_valid_clause cla level_asgns 0) then cla
  else diagnose (List.tl cla_hist) level_asgns (and_clause cla @@ List.hd cla_hist)
;;

let rec print_list =function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

(* 項に登場する変数のうち、もっとも早い登場が早いレベル *)
let check_level cla asgns =
  if List.length cla = 1 then 0
  else
    let level = List.length asgns in
    let rec make_level_map asgns level m =
      match asgns with
      | [] -> m
      | asgn_list::tl -> make_level_map tl (level-1) @@ List.fold_left (fun m (s, b) -> LevelMap.add s level m) m asgn_list
    in
    let m = make_level_map asgns level (LevelMap.empty) in
    let levels = List.fold_left
      (fun levels lit ->
        let l = LevelMap.find (get_variable lit) m in
        if List.mem l levels then levels else l::levels)
      [] cla
    in
    if List.length levels = 1 then List.hd levels - 1 else List.nth (List.rev @@ List.sort comp_int levels) 1
;;