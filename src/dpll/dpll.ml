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
exception Sat of assign list;;
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

let apply_assign cnf1 asgn asgns =
  let (s, b) = asgn in
  let res = List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1 in
  if res = [] then raise (Sat (asgn :: asgns))
  else res
;;

let asgn_pos = 2;;
let asgn_neg = 1;;

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

let next_assign_list cnf1 alphs =
  match unit_propagation cnf1 with
  | Some (s, b) -> (s, [(s, b)])
  | None -> (List.hd alphs, [(List.hd alphs, false); (List.hd alphs, true)])
;;

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