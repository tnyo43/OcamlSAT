type assign = int * bool;;

type literal = int;;

type clause = literal list;;
type cnf = clause list;;

module IntOrd = struct
  type t = int
  let compare = Pervasives.compare
end
module SS = Set.Make(IntOrd);;

exception Satisfied;;
exception Unsat;;
exception Conflict;;
exception ConflictClause of clause;;



(* literal functions *)



let pos x = Pervasives.abs x;;
let neg x = -1 * (Pervasives.abs x);;
let get_variable lit = if lit > 0 then lit else -lit;;
let get_state lit = (lit > 0);;

let int_of_lit lit =
  let s = get_variable lit in
  let b = if get_state lit then 1 else 0 in
  s*2 + b
;;



(* compare funtions *)



let comp_int s1 s2 =
  if s1 = s2 then 0
  else if s1 > s2 then 1
  else -1
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

let sort_assign asgns =
  List.sort (fun asgn1 asgn2 ->
    match (asgn1, asgn2) with
    | (x, _), (y, _) -> if x > y then 1 else if x = y then 0 else (-1)) asgns
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


(* clause functions *)



let update_clause cla s b =
    if List.length cla = 1
    then
      let lit = List.hd cla in
      if get_variable lit = s
      then
        if get_state lit = b then [] else raise Conflict
      else cla
    else
    let rec check cla res =
      match cla with
      | [] -> List.rev res
      | lit::cla' ->
            if get_variable lit = s then
              if get_state lit = b then [] else check cla' res
            else check cla' @@ lit::res
    in
    check cla []
;;



(* print functions *)



let rec print_list lst =
  match lst with
  | [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let rec print_list_list lsts = 
  match lsts with
  | [] -> ()
  | e::l -> print_list e; print_string "\n"; print_list_list l;
;;

let print_cla_hist_list cla_hist_list =
  let rec sub list num =
    match list with
    | [] -> ()
    | hd::tl -> print_int num; print_string "~~~ ->\n"; print_list_list hd; sub tl @@ num+1;
  in
  sub cla_hist_list 0
;;

let print_asgn asgn =
  let (s,b) = asgn in print_int s; print_string @@ ":"^string_of_bool b
;;

let rec print_asgn_list asgns =
  match asgns with
  | [] -> ()
  | asgn::t -> let _ = print_asgn asgn in let _ = print_string "," in print_asgn_list t
;;

let print_DB db =
  print_list_list db
;;

let print_clause cla = print_string "[" ; print_list cla; print_string "]";;


let rec print_cnf cnf1 =
  match cnf1 with
  | [] -> print_string "\n"
  | cla::t -> print_clause cla; print_cnf t
;;

let rec print_clause_hist_list cla_hist_list =
  match cla_hist_list with
  | [] -> ()
  | h::t -> let _ = print_cnf h in print_clause_hist_list t
;;



(* other functions  *)


let make_alph_set cnf1 =
  let add_set cla set = List.fold_right (fun lit -> SS.add @@ get_variable lit) cla set in
  List.fold_right (fun cla -> add_set cla) cnf1 SS.empty
;;


let checker cnf1 asgns =
  let checker_sub cla asgns =
    try
      let res = List.fold_left (fun cla (s,b) -> update_clause cla s b) cla asgns in
      res = []
    with
    | Conflict -> false
  in
  List.fold_left (fun res cla -> res && checker_sub cla asgns) true cnf1
;;


let rec zip a b =
  match a, b with
  | [], _ -> []
  | _, [] -> []
  | ha::ta, hb::tb -> (ha,hb) :: zip ta tb
;;