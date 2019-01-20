open Dpll

module SudokuCell = Map.Make(IntOrd);;

let range1 m n =
  let rec range i j =
    if i > j then []
    else i :: (range (i+1) j)
  in
  range m n
;;

let range n = range1 1 n;;

let isqrt n =
  let rec isqrt n i =
    if i*i > n then i-1
    else isqrt n (i+1)
  in
  isqrt n 1
;;

let variable_of_cell_and_num n i j k = (k-1) + n*((j-1) + n*n*(i-1));;

let cell_and_num_of_variable n s = [s/n/n+1; ((s/n) mod n) + 1; (s mod n) + 1];;

let number_of_asgn_alphabet n s =
  List.nth (cell_and_num_of_variable n s) 2
;;

let init_for_cell n res =
  let rec init_for_cell m res l =
    if m = 0 then res
    else init_for_cell (m-1) ((List.fold_right (fun x cla -> P (variable_of_cell_and_num n l m x)::cla) (range n) []) :: res) l
  in
  List.fold_right (fun l res -> ((init_for_cell n [] l))@ res) (range n) res
;;

let init_sub n f1 res =
  let rec init_sub z res l =
    if z = 0 then res
    else
      init_sub (z-1) (List.fold_right
        (fun x cnf1 ->
          (List.map (f1 l x z) (range n)) @ cnf1)
        (range (n-z)) res) l
  in
  List.fold_right (fun l res -> init_sub n res l) (range n) res
;;

let init_line n res =
  init_sub
    n
    (fun l x z -> (fun y -> [N (variable_of_cell_and_num n l x y); N (variable_of_cell_and_num n l (x+z) y)]))
    res
;;

let init_column n res =
  init_sub
    n
    (fun c x z -> (fun y -> [N (variable_of_cell_and_num n x c y); N (variable_of_cell_and_num n (x+z) c y)]))
    res
;;

let init_block n res =
  let s = isqrt n in
  let rec remove_u lst k =
    match lst with
    | [] -> k []
    | h::t ->
      begin
        match h with
        | None -> remove_u t k
        | Some h' -> remove_u t (fun x -> k (h'::x)) 
      end
  in
  remove_u (
    init_sub
      n
      (fun l x z -> (
        fun y -> 
          if ((l-1)/s*s+(x-1)/s+1) = ((l-1)/s*s+(x+z-1)/s+1) || ((l-1) mod s * s +(x-1) mod s+1) = ((l-1) mod s * s +(x+z-1) mod s+1) then None
          else Some [N (variable_of_cell_and_num n ((l-1)/s*s+(x-1)/s+1) ((l-1) mod s * s +(x-1) mod s+1) y);
                     N (variable_of_cell_and_num n ((l-1)/s*s+(x+z-1)/s+1) ((l-1) mod s * s +(x+z-1) mod s+1) y)])
      )
      res
  ) (fun x -> x)
;;

let init_sudoku n =
  init_for_cell n @@ init_line n @@ init_column n @@ init_block n []
;;

let create_problem problem n =
  let rec create_line line i j res =
    match line with
    | [] -> res
    | h::t ->
      if h = 0 then create_line t i (j+1) res
      else create_line t i (j+1) ([P (variable_of_cell_and_num n i j h)] :: res)
  in
  let rec create field i res =
    match field with
    | [] -> res
    | h::t ->
      create t (i+1) @@ create_line h i 1 res
  in create problem 1 @@ init_sudoku n
;;

let solve_sudoku problem n =
  let asgn = solve @@ create_problem problem n in
  let rec puzzle_of_asgn asgn k =
    match asgn with
    | [] -> k []
    | (s, false)::t -> puzzle_of_asgn t k
    | (s, true)::t -> puzzle_of_asgn t (fun puzzle -> k (number_of_asgn_alphabet n s::puzzle))
  in
  let rec n_split i inner lst k =
    match lst with
    | [] -> if i = 1 then k [] else failwith "the length of list must be multiple of n"
    | h::t -> if i = n then n_split 1 [] t (fun res -> k ((List.rev (h::inner))::res)) else n_split (i+1) (h::inner) t k
  in
  let puzzle = puzzle_of_asgn asgn (fun x -> x) in
  n_split 1 [] puzzle (fun x -> x)
;;


let problem = [
  [0;0;0;4];
  [0;0;1;2];
  [0;0;4;3];
  [4;3;2;1]
];;

solve_sudoku problem 4;;