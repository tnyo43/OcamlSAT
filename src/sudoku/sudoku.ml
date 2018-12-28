open Dpll

module SudokuCell = Map.Make(String);;

let alphabet_of_int n = char_of_int @@ (int_of_char 'a') + n - 1;;
let int_of_alphabet a = (int_of_char a) - (int_of_char 'a') + 1;;

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

let alphabet_of_cell_and_num i j k =
  List.fold_left (fun s -> fun n -> s^(Char.escaped @@ alphabet_of_int n)) "" [i; j; k]
;;

let cell_and_num_of_alphabet s =
  List.map (fun i -> String.get s i)(range 3)
;;

let init_for_cell n res =
  let rec init_for_cell m res l =
    if m = 0 then res
    else init_for_cell (m-1) ((List.fold_right (fun n cla -> P (alphabet_of_cell_and_num l m n)::cla) (range n) []) :: res) l
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
    (fun l x z -> (fun y -> [N (alphabet_of_cell_and_num l x y); N (alphabet_of_cell_and_num l (x+z) y)]))
    res
;;

let init_column n res =
  init_sub
    n
    (fun c x z -> (fun y -> [N (alphabet_of_cell_and_num x c y); N (alphabet_of_cell_and_num (x+z) c y)]))
    res
;;

let init_block n res =
  let s = isqrt n in
  let rec remove_u lst k =
    match lst with
    | [] -> k []
    | h::t -> if h = [U] then remove_u t k else remove_u t (fun x -> k (h::x))
  in
  remove_u (
    init_sub
      n
      (fun l x z -> (
        fun y -> 
          if ((l-1)/s*s+(x-1)/s+1) = ((l-1)/s*s+(x+z-1)/s+1) || ((l-1) mod s * s +(x-1) mod s+1) = ((l-1) mod s * s +(x+z-1) mod s+1) then [U]
          else [N (alphabet_of_cell_and_num ((l-1)/s*s+(x-1)/s+1) ((l-1) mod s * s +(x-1) mod s+1) y); N (alphabet_of_cell_and_num ((l-1)/s*s+(x+z-1)/s+1) ((l-1) mod s * s +(x+z-1) mod s+1) y)])
      )
      res
  ) (fun x -> x)
;;
