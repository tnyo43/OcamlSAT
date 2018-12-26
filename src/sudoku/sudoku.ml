
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

let cell_and_num_to_alphabet i j k =
	List.fold_left (fun s -> fun n -> s^(Char.escaped @@ alphabet_of_int n)) "" [i; j; k]
;;

let update sudoku i j k b cells =
	SudokuCell.update (cell_and_num_to_alphabet i j k) (fun _ -> Some 0) cells
;;

let init_for_cell n res =
	let rec init_for_cell m res l =
		if m = 0 then res
		else init_for_cell (m-1) ((List.fold_right (fun n cla -> (cell_and_num_to_alphabet l m n)::cla) (range n) []) :: res) l
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
		(fun l x z -> (fun y -> [(cell_and_num_to_alphabet l x y); (cell_and_num_to_alphabet l (x+z) y)]))
		res
;;

let init_column n res =
	init_sub
		n
		(fun c x z -> (fun y -> [(cell_and_num_to_alphabet x c y); (cell_and_num_to_alphabet (x+z) c y)]))
		res
;;
