type assign = string * bool;;

type literal = | P of string | N of string | U;;
type clause = literal list;;
type cnf = clause list;;

type alphabets = string list;;

module SS = Set.Make(String);;

exception Satisfied;;
exception Unsat;;
exception Sat of assign list;;
exception Test of cnf;;

let default_assign = false;;

let get_variable lit =
  match lit with
  | P s -> s
  | N s -> s
  | U -> failwith "get variavle from invalid literal"
;;

let get_state lit =
  match lit with
  | P _ -> true
  | N _ -> false
  | U -> failwith "get state from invalid literal"
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
    let cla1 = List.map
      (fun lit ->
        if s = get_variable lit then
          if b = get_state lit then raise Satisfied else U
        else lit)
      cla
    in
    if cla1 = [U] then raise Unsat
    else List.filter (fun lit -> not (lit = U)) cla1
  with | Satisfied -> []
;;

let apply_assign cnf1 asgn asgns =
  let (s, b) = asgn in
  let res = List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1 in
  if res = [] then raise (Sat (asgn :: asgns))
  else res
;;

let next_assign_list sorted_cnf alphs =
  if List.length @@ List.hd sorted_cnf = 1 then
    let lit = List.hd @@ List.hd sorted_cnf in
    (get_variable lit, [(get_variable lit, get_state lit)])
  else (List.hd alphs, [(List.hd alphs, false); (List.hd alphs, true)])
;;

let solve cnf1 =
  let rec solve cnf1 alphs asgns =
    let sorted_cnf = List.sort (fun l1 l2 -> (List.length l1) - (List.length l2)) cnf1 in
    let (a, next_assigns) = next_assign_list sorted_cnf alphs in
    let next_alphs = List.filter (fun x -> not(x = a)) alphs in
    let rec try_asgn next_assigns =
      match next_assigns with
      | [] -> raise Unsat
      | asgn::tl ->
          try
            let cnf2 = apply_assign sorted_cnf asgn asgns in
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