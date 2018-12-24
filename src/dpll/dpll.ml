type assign = string * bool;;

type literal = | P of string | N of string;;
type clause = literal list;;
type cnf = clause list;;

type alphabets = string list;;

exception Satisfied;;
exception Unsat;;

let get_variable lit =
  match lit with
  | P s -> s
  | N s -> s
;;

let get_state lit =
  match lit with
  | P _ -> true
  | N _ -> false
;;

let update_clause cla s b =
  let rec judge h t k =
    if s = get_variable h then
      if b = get_state h then raise Satisfied
      else k t
    else update t (fun x -> k (h::x))
  and update cla k =
    match cla with
    | [] -> k []
    | h::t -> judge h t k
  in
  let update_single_clause cla b =
    let b1 = get_state @@ List.hd cla in
    if b = b1 then [] else raise Unsat
  in
    if List.length cla = 1 && get_variable (List.hd cla) = s
    then update_single_clause cla b
    else 
      try update cla (fun x -> x)
      with | Satisfied -> []
;;

let apply_assign cnf1 asgn =
  let (s, b) = asgn in
  List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1
;;

