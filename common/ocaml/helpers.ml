exception Error of string;;
exception Empty_list of string;;

(* ===== ARITHMETICS ===== *)
let add a b = a + b;;
let mul a b = a * b;;
let sub a b = a - b;;
let div a b = a / b;;

(* ===== ARRAY OPERATIONS ===== *)
(* find index of first element in array that matches pred *)
let arr_findi_opt pred arr =
	let len = Array.length arr in
		let rec iter n = 
			if n = len then -1 else if pred arr.(n) then n else iter (n + 1)
		in iter 0;;
	

(* ===== LIST OPERATIONS ===== *)
let list_empty lst = 
    match lst with
        | [] -> true
        | _ -> false;;

let filter pred lst =
    List.fold_left (fun init el -> if pred el then init @ [el] else init) [] lst;;

let filter_not pred lst = filter (fun x -> not (pred x)) lst

let filter_empty lst = filter_not (fun x -> x = []) lst

let list_split lst sep =
    let rec iter sublst res rest = 
        match rest with
            | [] -> res @ (filter_empty [sublst])
            | h :: t -> if h = sep then iter [] (res @ (filter_empty [sublst])) t
                        else iter (sublst @ [h]) res t
    in
    iter [] [] lst;;

(* take first n elements from lst *)
let rec take n lst =
    match lst with
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n - 1) t;;

let list_max lst =
    match lst with
        | [] -> raise (Empty_list "list_max expects a non-empty list")
        | h :: t -> List.fold_left max h t;;

let list_sum lst = 
    match lst with
        | [] -> raise (Empty_list "list_sum expects a non-empty list")
        | h :: t -> List.fold_left (fun x y -> x + y) h t;;

let rec find_first pred lst =
    match lst with
        | [] -> None
        | h :: t -> if pred h then Some h else find_first pred t;;

(* split a list into sublists of size n *)
let list_split_n n lst =
	let rec iter curr i result rest =
		match rest with
			| [] -> result @ (filter_empty [curr])
			| h :: t -> if i = n then iter [h] 1 (result @ [curr]) t
						else iter (curr @ [h]) (i+1) result t
	in iter [] 0 [] lst;;

let list_sort_desc lst = List.sort (fun x y -> (compare x y) * -1) lst;;
let list_sort_asc lst = List.sort compare lst;;

let string_to_list s = String.fold_left (fun res c -> res @ [c]) [] s;;

(* ===== FILE (IO) OPERATIONS ===== *)
let read_lines chan =
    let rec iter lst =
        match (In_channel.input_line chan) with
            | None -> lst
            | Some(ln) -> iter (lst @ [ln])
    in
    iter [];;

let file_to_lines filename =
    In_channel.with_open_text filename read_lines;;

(* ===== MISCELLANEOUS ===== *)
let inside x min max = x > min && x < max;;
let around x min max = x >= min && x <= max;;
