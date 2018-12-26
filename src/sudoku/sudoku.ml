open Dpll

type sudoku = (int list) list;;

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

let cell_and_num_to_alphabet i j k =
	List.fold_left (fun s -> fun n -> s^(Char.escaped @@ alphabet_of_int n)) "" [i; j; k]
;;

let update sudoku i j k b cells =
	SudokuCell.update (cell_and_num_to_alphabet i j k) (fun _ -> Some 0) cells
;;

let init_for_cell n res =
	let rec init_for_cell m res l =
		if m = 0 then res
		else init_for_cell (m-1) ((List.fold_right (fun n cla -> P (cell_and_num_to_alphabet l m n)::cla) (range n) []) :: res) l
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
		(fun l x z -> (fun y -> [N (cell_and_num_to_alphabet l x y); N (cell_and_num_to_alphabet l (x+z) y)]))
		res
;;

let init_column n res =
	init_sub
		n
		(fun c x z -> (fun y -> [N (cell_and_num_to_alphabet x c y); N (cell_and_num_to_alphabet (x+z) c y)]))
		res
;;

let init_block n res =
	let s = isqrt n in
	init_sub
		n
		(fun l x z -> (fun y -> [N (cell_and_num_to_alphabet ((l-1)/s*s+(x-1)/s+1) ((l-1) mod s * s +(x-1) mod s+1) y); N (cell_and_num_to_alphabet ((l-1)/s*s+(x+z-1)/s+1) ((l-1) mod s * s +(x+z-1) mod s+1) y)]))
		res
;;

let init_sudoku n =
	init_for_cell n @@ init_line n @@ init_column n @@ init_block n []
;;