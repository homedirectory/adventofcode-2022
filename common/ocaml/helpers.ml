exception Error of string;;
exception Empty_list of string;;
exception Out_of_bounds;;

(* ========== ARITHMETICS ========== *)
let add a b = a + b;;
let mul a b = a * b;;
let sub a b = a - b;;
let div a b = a / b;;
let pow a e = Int.of_float ((Int.to_float a) ** (Int.to_float e));;
let inc x = x + 1;;
let dec x = x - 1;;

(* ========== MISCELLANEOUS ========== *)
let inside x min max = x > min && x < max;;
let around x min max = x >= min && x <= max;;

let int_of_bool b = if b then 1 else 0;;

(* ========== CHAR OPERATIONS ========== *)
let isupper c = around (Char.code c) 65 90;;
let is_ascii c = 
    let code = (Char.code c) in
    (around code 65 90) || (around code 97 122);;

let atoi c =
    let code = Char.code c in
    if around code 48 57 then code - 48
    else raise (Error "atoi: invalid char");;

(* ========== STRING OPERATIONS ========== *)
let str_from (n:int) s = String.sub s n (String.length s - n);;
let string_to_list s = String.fold_right List.cons s [];;
let str_explode s = string_to_list s;;

let string_findi f s =
    let len = String.length s in
    let i, stop = ref 0, ref false in
    while not !stop && !i < len do
        if f !i then stop := true else i := !i + 1
    done;
    if not !stop then None else Some !i;;


(* ========== ARRAY OPERATIONS ========== *)
(* find index of first element in array that matches pred *)
let arr_findi_opt pred arr =
	let len = Array.length arr in
		let rec iter n = 
			if n = len then -1 else if pred arr.(n) then n else iter (n + 1)
		in iter 0;;
	

(* ========== LIST OPERATIONS ========== *)
let range left right =
    if left >= right then raise (Error "invalid range")
    else
        let rec iter n =
            if n = right
            then []
            else n :: (iter (n+1))
        in
        iter left;;

let list_empty lst = 
    match lst with
        | [] -> true
        | _ -> false;;

let rec list_last lst =
    match lst with
    | [] -> raise (Error "list_last expects a non-empty list")
    | h :: [] -> h
    | h :: t -> list_last t;;

let list_min lst =
    match lst with
    | [] -> raise (Error "list_min received empty list")
    | h::t -> List.fold_left Int.min h t;;

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

let rec drop n lst =
    match lst with
    | [] -> if n = 0 then [] else raise Out_of_bounds
    | h :: t -> if n = 0 then lst else drop (n-1) t;;

let list_max lst =
    match lst with
        | [] -> raise (Empty_list "list_max expects a non-empty list")
        | h :: t -> List.fold_left max h t;;

let list_min lst = 
    match lst with
        | [] -> raise (Empty_list "list_min expects a non-empty list")
        | h :: t -> List.fold_left min h t;;

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

let list_every_nth n lst =
    List.filteri (fun i x -> i mod n ==== 0) lst;;

let split_after n lst =
    let rec iter i left right =
        if i > n then (left, right)
        else match right with
        | [] -> raise Out_of_bounds
        | h :: t -> iter (i+1) (left @ [h]) t
    in iter 0 [] lst;;

let split_before n lst =
    split_after (n-1) lst;;

let transpose lst = 
    List.of_seq @@ Seq.map List.of_seq @@ Seq.transpose @@ List.to_seq @@ List.map List.to_seq lst;;

let sq_range left right =
    Seq.take (right - left) @@ Seq.ints left;;

let sq_indexize sq =
    Seq.mapi (fun i x -> (i, x)) sq;;

let sq_findi f sq =
    match Seq.find (fun (i, x) -> f x) (sq_indexize sq) with
    | None -> None
    | p -> p;;

let list_drop_right n lst =
    let stop_at = List.length lst - n in
    let rec iter i tail =
        if i = stop_at then []
        else match tail with 
        | h::t -> h :: iter (i+1) t
        | [] -> raise Out_of_bounds
    in iter 0 lst;;

(* remove n elements from both sides of lst *)
let list_strip_n n lst =
    drop 1 (list_drop_right 1 lst);;

let rec take_while pred lst =
    match lst with
    | [] -> []
    | h :: t -> if pred h then h :: (take_while pred t) else [];;

let rec stop_after pred lst =
    match lst with
    | [] -> []
    | h :: t -> if pred h then [h] else h :: (stop_after pred t);;

let rec list_windowed n lst =
    match lst with
    | [] -> []
    | _ -> if n >= (List.length lst)
           then [lst]
           else (match (split_after (dec n) lst) with
                 | wind, rest -> wind :: (list_windowed n rest));;

let list_picki indexes lst =
    List.map (fun i -> (i, List.nth lst i)) indexes;;

(* ========== FILE (IO) OPERATIONS ========== *)
let read_lines chan =
    let rec iter lst =
        match (In_channel.input_line chan) with
            | None -> lst
            | Some(ln) -> iter (lst @ [ln])
    in
    iter [];;

let file_to_lines filename =
    In_channel.with_open_text filename read_lines;;

let file_to_text filename =
    In_channel.with_open_text filename In_channel.input_all;;
