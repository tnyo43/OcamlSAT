open Sat

type cdcl_clause = clause * clause;;
type cdcl_cnf = cdcl_clause list;;

module LevelMap = Map.Make(IntOrd);;

exception CdclSat of assign list;; (* 最後に呼ばれる *)
exception UnitPropagation of (literal * clause);;

let create_cdcl_cnf cnf1 =
  List.map (fun cnf' -> (cnf',cnf')) cnf1
;;


let rec print_cdcl_cnf (cdcl_cnf1 : cdcl_cnf) =
  match cdcl_cnf1 with
  | [] -> print_string "\n"
  | (cla, _)::t -> print_clause cla; print_cdcl_cnf t

let rec print_cnf_hist (cnf_hist : cdcl_cnf list) =
  match cnf_hist with
  | [] -> ()
  | h::t -> let _ = print_cdcl_cnf h in print_cnf_hist t
;;


let show_state
        (asgn_level_list : (assign list) list) (* 各レベルで割り当てた結果 *)
        (cnf_hist : cdcl_cnf list) (* 各レベルでまだ解決していないCNFの残り *)
        (cla_hist_list : (clause list) list) (* 各レベルで使用した項のリスト *)
        (clause_db : clause list) (* CNFを構成する項全体 *)
        alphs (* 割り当てられていない変数 *)
        initial_alphs = (* CNFに登場する全ての変数 *)
    let _ = print_string "\n       ==== state ===\n" in
    let _ = print_string "===== assign level list ====\n" in
    let _ = List.iter (fun x -> print_asgn_list x; print_string "\n") asgn_level_list in
    let _ = print_string "========= CNF hist =========\n" in
    let _ = print_cnf_hist cnf_hist in
    let _ = print_string "===== clause hist list =====\n" in
    let _ = print_clause_hist_list cla_hist_list in
    let _ = print_string "======== clause DB =========\n" in
    let _ = print_DB clause_db in
    let _ = print_string "===== remain variable ======\n" in
    let _ = print_list alphs in
    let _ = print_string "\n===== initial variable =====\n" in
    let _ = print_list initial_alphs in
    let _ = print_string "\n============================\n" in
        ()
;;

let and_clause cla1 cla2 ignore_list = (* ignore_listに入っている変数はすでにPNの両方が出ている *)
  let cla1' = List.filter (fun x -> not (List.mem (get_variable x) ignore_list)) cla1 in
  let cla2' = List.filter (fun x -> not (List.mem (get_variable x) ignore_list)) cla2 in
  let lit_list = List.sort comp_literal (cla1'@cla2') in
  let rec union lst res ignore_list =
    match lst with
    | [] -> List.rev res, ignore_list
    | h::[] -> h::res, ignore_list
    | h1::h2::t ->
        if h1 = h2 then union t (h2::res) ignore_list
        else if h1 = -1 * h2 then union t res (get_variable h1::ignore_list)
        else union (h2::t) (h1::res) ignore_list
  in
  let new_clause, new_ignore_list = union lit_list [] ignore_list in
  if List.length new_clause = 0 then raise Conflict else new_clause, new_ignore_list (* Conflictに変更して、衝突の原因になった項を追加する *)
;;

let apply_assign_to_cnf cnf1 asgn asgn_level_list =
  let (s, b) = asgn in
  let res = List.filter (fun (cla, _) -> List.length cla > 0) 
         @@ List.map (fun (cla, original_cla) -> 
                try
                  update_clause cla s b, original_cla
                with Conflict -> raise (ConflictClause original_cla)) cnf1
  in
  if List.length res = 0 then raise (CdclSat (asgn :: List.flatten asgn_level_list))
  else res
;;

(* 単位伝播があるとき、その伝播をもたらした元々の項を教える *)
let unit_propagation cnf1 =
  try
    let _ = List.iter
              (fun (current_cla, original_cla) ->
                if List.length current_cla = 1
                then let lit = List.hd current_cla in raise (UnitPropagation (lit, original_cla))
                else ())
              cnf1 in
    None
  with | UnitPropagation res -> Some res
;;

let next_assign cnf1 alphs =
  match unit_propagation cnf1 with
  | Some (lit, cla) -> let a = get_variable lit in (a, get_state lit), Some cla
  | None -> let a = List.hd alphs in (a, true), None
;;

let add_in_same_level obj_level_list obj =
  match obj_level_list with
  | [] -> failwith "add in same level"
  | hd::tl -> ((obj::hd)::tl)
;;

let add_in_next_level obj_level_list obj =
  [obj]::obj_level_list
;;

let rec is_valid_clause cla asgn_level count =
  match asgn_level with
  | [] -> count <= 1
  | (s, b)::tl -> is_valid_clause cla tl @@
      List.fold_left (fun c lit -> if get_variable lit = s then c+1 else c) count cla
;;

let diagnose (cla_list : clause list) (asgn_level : assign list) conflict_cla cause_cla =
(* 単位伝播でcause clauseが呼ばれてconflict clauseが衝突した *)
  let rec diagnose_sub (cla_list : clause list) (asgns : assign list) (cla : clause) (ignore_list : literal list) =
    if is_valid_clause cla asgn_level 0 then cla
    else if List.length cla_list = 0 then let _ = print_clause cla in let _ = print_list ignore_list in raise Conflict
    else 
      let (s,_) = List.hd asgns in
      if List.mem s cla || List.mem (-s) cla then
        let new_clause, new_ignore_list = and_clause cla (List.hd cla_list) ignore_list in
        diagnose_sub (List.tl cla_list) (List.tl asgns) new_clause new_ignore_list
      else diagnose_sub (List.tl cla_list) (List.tl asgns) cla ignore_list
  in
  let first_clause, ignore_list = and_clause conflict_cla cause_cla [] in
  diagnose_sub cla_list asgn_level first_clause ignore_list
;;

let check_level cla asgn_level_list =
  if List.length cla = 1 then 0 (* literalが一つしかない項が出たら追加して最初に戻る *)
  else
    let level = List.length asgn_level_list in
    let rec make_level_map asgn_level_list level m =
      match asgn_level_list with
      | [] -> m
      | asgn_list::tl -> make_level_map tl (level-1) @@ List.fold_left (fun m (s, b) -> LevelMap.add s level m) m asgn_list
    in
    let m = make_level_map asgn_level_list level (LevelMap.empty) in

    let levels = List.fold_left
      (fun levels lit ->
        let l = LevelMap.find (get_variable lit) m in
        if List.mem l levels then levels else l::levels)
      [] cla
    in
    if List.length levels = 1 then List.hd levels - 1 else List.nth (List.rev @@ List.sort comp_int levels) 1
;;

let update_level_asgns asgns asgn =
  match asgns with
  | hd::tl -> (asgn::hd) :: tl
;;

let rec back_to_level level lst =
  if List.length lst = level then lst
  else back_to_level level @@ List.tl lst
;;

let eliminate_used_alph all_alphs used_alphs =
  List.filter (fun x -> not (List.mem x used_alphs)) all_alphs
;;


let rec apply_and_update f lst x =
  match lst with
  | [] -> []
  | h::t -> let y = f x h in y::apply_and_update f t y
;;


let update_clause_with_assigns asgn_level_list cla =
  let rev_asgn_list = List.rev asgn_level_list in
  let rec apply_asgns cla asgns =
    match asgns with
    | [] -> cla
    | h::t -> let (s,b) = h in let cla1 = update_clause cla s b in apply_asgns cla1 t
  in
  List.rev @@ apply_and_update apply_asgns rev_asgn_list cla
;;

let add_clause_every_level asgn_level_list cla cnf_hist =
  let cla_level = update_clause_with_assigns asgn_level_list cla in
  List.map (fun (updated_cla,cnf) -> (updated_cla,cla)::cnf) @@ zip cla_level cnf_hist 
;;

let rec solve_sub
        (asgn_level_list : (assign list) list) (* 各レベルで割り当てた結果 *)
        (cnf_hist : cdcl_cnf list) (* 各レベルでまだ解決していないCNFの残り *)
        (cla_hist_list : (clause list) list) (* 各レベルで使用した項のリスト *)
        (clause_db : clause list) (* CNFを構成する項全体 *)
        alphs (* 割り当てられていない変数 *)
        initial_alphs = (* CNFに登場する全ての変数 *)
  let asgn, cla' = next_assign (List.hd cnf_hist) alphs in
  let (s, b) = asgn in
  let next_alphs = List.filter (fun x -> not(x = s)) alphs in
  match cla' with
  | Some cla -> (* 単位伝播 *)
    begin
      let new_asgn_level_list,
          new_cnf_hist,
          new_cla_hist_list,
          new_clause_db,
          new_next_alph,
          sat = begin
              (* 要素数をそれぞれNとする(N >= 1) *)
                try
                  let new_cnf = apply_assign_to_cnf (List.hd cnf_hist) asgn asgn_level_list in
                  let new_cnf_hist = new_cnf :: List.tl cnf_hist in
                  let new_asgn_level_list = add_in_same_level asgn_level_list asgn in
                  let new_cla_hist_list = add_in_same_level cla_hist_list cla in (* Nのまま *)
                   (new_asgn_level_list,
                    new_cnf_hist,
                    new_cla_hist_list,
                    clause_db,
                    next_alphs,
                    false)
                with
                | CdclSat asgn_list ->
                          [asgn_list],
                          [[]],
                          [[]],
                          [],
                          initial_alphs,
                          true
                | ConflictClause cla2 -> (* 単位伝播した結果失敗 *)
                  if List.length cla_hist_list <= 1 then raise Unsat (* 1つ目の適応でうまくいかないときはだめ *)
                  else
                    let new_clause = diagnose (List.hd cla_hist_list) (List.hd asgn_level_list) cla cla2 in
                    let new_clause_db = new_clause::clause_db in
                    let level = check_level new_clause @@ update_level_asgns asgn_level_list asgn in
                    if level = 0 then (* 追加して最初の状態に戻る *)
                          [[]],
                          [create_cdcl_cnf new_clause_db],
                          [[]],
                          new_clause_db,
                          initial_alphs,
                          false (* それぞれ長さ1 *)
                    else
                      let new_asgn_level_list = back_to_level level asgn_level_list in
                      let new_cnf_hist = add_clause_every_level new_asgn_level_list new_clause @@ back_to_level level cnf_hist in (* CNFに新しい項を追加する *)
                      let new_next_alph = eliminate_used_alph initial_alphs @@ List.map (fun (s, b) -> s) @@ List.flatten new_asgn_level_list in
                      let new_cla_hist_list = back_to_level level cla_hist_list in (* それぞれ同じ数だけ戻った *)
                          new_asgn_level_list,
                          new_cnf_hist,
                          new_cla_hist_list,
                          new_clause_db,
                          new_next_alph,
                          false

          end
      in
      if sat then sort_assign @@ List.flatten new_asgn_level_list
      else solve_sub new_asgn_level_list new_cnf_hist new_cla_hist_list new_clause_db new_next_alph initial_alphs

    end
  | None -> (* 決め打ちの代入、レベルが一つ進む *)
    let new_cnf_hist, sat_result = begin
      try
        (apply_assign_to_cnf (List.hd cnf_hist) asgn asgn_level_list)::cnf_hist, None
      with
      | CdclSat asgn_list -> [], Some asgn_list
    end in
    match sat_result with
    | Some asgn_list -> asgn_list
    | None -> 
      let new_cla_hist_list = List.hd cla_hist_list::cla_hist_list in
      let new_asgn_level_list = add_in_next_level asgn_level_list asgn in
      solve_sub new_asgn_level_list new_cnf_hist new_cla_hist_list clause_db next_alphs initial_alphs
;;

let rec solve cnf1 = 
  let alph_set = make_alph_set cnf1 in
  let alphs = SS.elements alph_set in
  let asgns = solve_sub [[]] [create_cdcl_cnf cnf1] [[]] cnf1 alphs alphs (* 要素数がそれぞれ1 *) in
  let rests = SS.elements @@ List.fold_right (fun (s, _) -> SS.remove s) asgns alph_set in
  let result_assigns = List.fold_left (fun lst s -> (s, false)::lst) asgns rests in  
  let result = sort_assign result_assigns in
  result
;;
