type assign = string * bool;;

type literal = | P of string | N of string | U;;
type clause = literal list;;
type cnf = clause list;;

type alphabets = string list;;

exception Satisfied;;
exception Unsat;;

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

let apply_assign cnf1 asgn =
  let (s, b) = asgn in
  List.filter (fun cla -> List.length cla > 0) @@ List.map (fun cla -> update_clause cla s b) cnf1
;;

