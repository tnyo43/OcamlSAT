let read_line ic =  Str.split (Str.regexp "[ \t]+") @@ input_line ic;;

let rec problem_size ic =
  let chars = read_line ic in
  if List.hd chars = "p" then
    let res = (int_of_string @@ List.nth chars 2, int_of_string @@ List.nth chars 3) in
    res
  else problem_size ic
;;

let rec read_problem ic num problem =
  if num = 0 then problem
  else
    let clause = List.rev @@ List.tl @@ List.rev @@ List.map int_of_string @@ read_line ic in
    read_problem ic (num-1) @@ clause::problem
;;

let make_dimacs_problem filename =
  let ic = open_in filename in
  try
    let (var_num, clause_num) = problem_size ic in
    let problem = read_problem ic clause_num [] in
    let _ = close_in ic in
    var_num, clause_num, problem
  with e ->
    close_in_noerr ic;
    raise e
;;

let trial problem solve_function =
  let t = Sys.time() in
  let asgns = solve_function problem in
  let execution_time = (Sys.time() -. t) in
  let judge = Dpll.checker problem asgns in
  let _ = Printf.printf "%f" execution_time in
  judge, execution_time
;;

let () =
  let filename = Sys.argv.(1) in
  let var_num, clause_num, problem = make_dimacs_problem filename in
  let test = trial problem in
  let _ = test Dpll.solve in
  let _ = print_string " " in
  let _ = test Cdcl.solve in
  print_string "\n";
  ()
