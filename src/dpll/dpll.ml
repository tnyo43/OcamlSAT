type assign = string * bool;;

type literal = | P of string | N of string;;
type clause = literal list;;
type cnf = clause list;;

type alphabets = string list;;

exception Satisfied;;

let update_clause cla s b =
  let rec judge s' h t b k =
    if s = s' then
      if b then raise Satisfied
      else k t
    else update t (fun x -> k (h::x))
  and update cla k =
    match cla with
    | [] -> k []
    | h::t ->
      begin
        match h with
        | P s' -> judge s' h t b k
        | N s' -> judge s' h t (not b) k
      end
  in
  try
    update cla (fun x -> x)
  with | Satisfied -> []
;;
