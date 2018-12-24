type assign = string * bool;;

type literal = | P of string | N of string | U;;
type clause = literal list;;
type cnf = clause list;;

type alphabets = string list;;

exception Satisfied;;
exception Unsat;;
exception Sat of assign list;;

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
    [(get_variable lit, get_state lit)]
  else [(List.hd alphs, false); (List.hd alphs, true)]
;;
