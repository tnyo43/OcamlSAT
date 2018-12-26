
type sudoku = (int list) list;;

module SudokuCell = Map.Make(String);;

let alphabet_of_int n = char_of_int @@ (int_of_char 'a') + n - 1;;
let int_of_alphabet a = (int_of_char a) - (int_of_char 'a') + 1;;

let range n =
	let rec range i j =
		if i > j then []
		else i :: (range (i+1) j)
	in
	range 1 n
;;

let cell_and_num_to_alphabet i j k =
	List.fold_left (fun s -> fun n -> s^(Char.escaped @@ alphabet_of_int n)) "" [i; j; k]
;;

let update sudoku i j k b cells =
	SudokuCell.update (cell_and_num_to_alphabet i j k) (fun _ -> Some 0) cells
;;

let init_sub n f1 f2 =
	let rec init_sub l res =
		if l = 0 then res
		else
			let rec init_sub_1 z res =
				if z = 0 then res
				else
					init_sub_1 (z-1) (List.fold_right
						(fun x cnf1 ->
							(List.map (f1 l x z) (range n)) @ cnf1)
						(range (n-z)) res)
			in
			let rec init_sub_2 m res =
				if m = 0 then res
				else init_sub_2 (m-1) ((List.fold_right (f2 l m) (range n) []) :: res)
			in
			init_sub (l-1) @@ init_sub_2 n @@ init_sub_1 (n-1) res
	in
	init_sub n []
;;

let init_line n = 
	init_sub
		n
		(fun l x z -> (fun y -> [(cell_and_num_to_alphabet l x y); (cell_and_num_to_alphabet l (x+z) y)]))
		(fun l m -> (fun n cla -> (cell_and_num_to_alphabet l m n)::cla))
;;

let init_column n =
	init_sub
		n
		(fun c x z -> (fun y -> [(cell_and_num_to_alphabet x c y); (cell_and_num_to_alphabet (x+z) c y)]))
		(fun c m -> (fun n cla -> (cell_and_num_to_alphabet m c n)::cla))
;;

(*
let init_box s =
	let n = s*s in
	let init_box_1 i j =



let init_sudoku sudoku n =
	List.map (cell_and_num_to_alphabet i 1 1)
*)