open Printf;;

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

let lcm a b =
    let lrg, sml = if a > b then a, b else b, a in
    let rec iter acc =
        if acc mod sml = 0 then acc else iter (acc + lrg)
    in
    iter lrg;;

let quotient n d =
    int_of_float (Float.floor (float_of_int n) /. (float_of_int d));;

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

let str_of_chars chars = List.fold_left (fun acc c -> acc ^ (String.make 1 c)) 
    "" chars;;

let string_findi f s =
    let len = String.length s in
    let i, stop = ref 0, ref false in
    while not !stop && !i < len do
        if f !i then stop := true else i := !i + 1
    done;
    if not !stop then None else Some !i;;

let string_empty s =
    String.length s = 0;;

let re_grp n re str =
    let z = try Some (Str.search_forward re str 0) with e -> None in
    match z with 
    | None -> None
    | _ -> Some (Str.matched_group n str);;

let str_to_int s =
    Scanf.sscanf s "%d" Fun.id;;

(* ========== ARRAY OPERATIONS ========== *)
(* find index of first element in array that matches pred *)
let arr_findi_opt pred arr =
	let len = Array.length arr in
		let rec iter n = 
			if n = len then -1 else if pred arr.(n) then n else iter (n + 1)
		in iter 0;;

let print_matrix printer mtrx =
    Array.(iter (fun row -> iter (fun x -> printer x; printf "; ") row; printf "\n%!") mtrx);;

let print_matrixi printer mtrx =
    Array.(iteri (fun y row -> iteri (fun x el -> printer (x,y) el; printf "; ") row;
        printf "\n%!")
    mtrx);;

let matrix_map f mtrx =
    Array.(map (map f) mtrx);;

let matrix_mapi f mtrx =
    Array.(mapi (fun y row -> mapi (fun x el -> f (x, y) el) row) mtrx);;

let matrix_flatten mtrx =
    Array.(fold_left append [||] mtrx);;

let matrix_dims mtrx =
    Array.(length mtrx, length mtrx.(0));;

let matrix_iteri f mtrx =
    Array.(iteri (fun y row -> iteri (fun x el -> f (x, y) el) row) mtrx);;

let matrix_iter f =
    matrix_iteri (fun _ el -> f el);; 

let print_matrix prntr sep mtrx =
    Array.(iter (fun row -> 
        iter (fun x -> prntr x; print_string sep) row; print_newline()) 
    mtrx);;

(* ========== LIST OPERATIONS ========== *)
let car = List.hd;;
let cadr lst = List.nth lst 1;;
let cdr = List.tl;;

let list_to_pair = function
    | a1 :: a2 :: [] -> (a1, a2)
    | _ -> failwith "list_to_pair: expected a list of length 2";;

let range left right =
    if left >= right then raise (Error "invalid range");
    let rec iter n =
        if n = right
        then []
        else n :: (iter (n+1))
    in
    iter left;;

let list_empty = function
    | [] -> true
    | _ -> false;;

let print_list printer lst =
    let rec iter = function
        | [] -> printf "]\n"
        | h :: [] -> printer h; iter []
        | h :: t -> printer h; printf "; "; iter t
    in
    print_char '[';
    iter lst;;

let list_ind_optf f itm lst =
    let rec iter i = function
        | [] -> None
        | h :: t -> if f h itm then Some i else iter (i+1) t
    in
    iter 0 lst;;

let list_ind itm lst =
    match list_ind_optf (=) itm lst with
    | None -> raise Not_found
    | Some i -> i;;

let rec list_last = function
    | [] -> raise (Error "list_last expects a non-empty list")
    | h :: [] -> h
    | h :: t -> list_last t;;

let list_min = function
    | [] -> raise (Error "list_min received empty list")
    | h::t -> List.fold_left Int.min h t;;

let filter pred lst =
    List.fold_left (fun init el -> if pred el then init @ [el] else init) [] lst;;

let filter_not pred lst =
    filter (fun x -> not (pred x)) lst

let filter_empty lst = 
    filter_not (fun x -> x = []) lst

let list_split lst sep =
    let rec iter = function
        | sub, res, [] -> res @ (filter_empty [sub])
        | sub, res, h :: t -> 
            if h = sep then iter ([], (res @ (filter_empty [sub])), t)
            else iter ((sub @ [h]), res, t)
    in
    iter ([], [], lst);;

(* take first n elements from lst *)
let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n - 1) t;;

let drop n lst =
    let rec iter m = function
        | [] -> if m = 0 then [] else raise Out_of_bounds
        | h :: t -> if m = 0 then t else iter (m-1) t
    in
    iter (n-1) lst;;

let list_drop = drop;;

let list_max = function
    | [] -> raise (Empty_list "list_max expects a non-empty list")
    | h :: t -> List.fold_left max h t;;

let list_min = function
    | [] -> raise (Empty_list "list_min expects a non-empty list")
    | h :: t -> List.fold_left min h t;;

let list_sum = function
    | [] -> raise (Empty_list "list_sum expects a non-empty list")
    | h :: t -> List.fold_left (fun x y -> x + y) h t;;

let rec find_first pred = function
    | [] -> None
    | h :: t -> if pred h then Some h else find_first pred t;;

let list_sort_asc lst = List.sort compare lst;;

let list_sortf f =
    List.sort (fun a b -> compare (f a) (f b));;

let list_sortf_desc f =
    List.sort (fun a b -> (-1) * (compare (f a) (f b)));;

let list_sort_desc = list_sortf_desc Fun.id;;

let list_every_nth n lst =
    List.filteri (fun i x -> i mod n = 0) lst;;

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
        | h :: t -> h :: iter (i+1) t
        | [] -> raise Out_of_bounds
    in iter 0 lst;;

(* remove n elements from both sides of lst *)
let list_strip_n n lst =
    drop 1 (list_drop_right 1 lst);;

let rec take_while pred = function
    | [] -> []
    | h :: t -> if pred h then h :: (take_while pred t) else [];;

let rec stop_after pred = function
    | [] -> []
    | h :: t -> if pred h then [h] else h :: (stop_after pred t);;

let rec list_windowed n = function
    | [] -> []
    | lst -> if n >= (List.length lst)
           then [lst]
           else (match (split_after (dec n) lst) with
                 | wind, rest -> wind :: (list_windowed n rest));;

let list_pick indexes lst =
    let arr = Array.of_list lst in
    List.map (fun i -> arr.(i)) indexes;;

let list_picki indexes lst =
    List.combine (list_pick indexes lst) indexes;;

(* generates a list of length n, where each value is derived from the previous one *)
let list_generate f init n =
    List.rev @@
    List.fold_left (fun acc _ -> 
        match acc with h :: t -> (f h) :: h :: t)
    [init]
    (range 0 n);;

(* similar to Seq.unfold but each element is derived just from the previous one *)
let list_unfold f init =
    let rec iter prev =
        match f prev with
        | None -> []
        | Some x -> x :: iter x
    in
    init :: iter init;;

let list_unfoldi f init =
    let rec iter i prev =
        match f i prev with
        | None -> []
        | Some x -> x :: iter (i+1) x
    in
    init :: iter 0 init;;

let list_add = function
    | [] -> []
    | h :: t -> List.fold_left (fun acc l -> List.map2 (+) acc l) h t;;

let list_rmi n lst =
    let rec iter i = function
        | [] -> raise (Error "list_rmi: out of bounds")
        | h :: t -> if i = n then t else h :: (iter (inc i) t)
    in
    match lst with
    | [] -> raise (Error "list_rmi expects non-empty list")
    | l -> iter 0 l;;

let list_rmis is lst =
    let rec iter n = function
        | [], l -> l
        | ih::it, [] -> raise (Error "list_rmi: out of bounds")
        | ih::it, h::t -> if ih = n then (iter (inc n) (it, t))
            else h :: (iter (inc n) (ih::it, t))
    in
    match is, lst with
    | h::t, [] -> raise (Error "list_rmis expects non-empty list")
    | is, lst -> iter 0 ((List.sort Int.compare is), lst);;

let list_lcm = function
    | [] -> raise (Error "list_lcm expects non-empty list")
    | h :: t -> List.fold_left lcm h t;;

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

let clean_lines lines =
    List.map String.trim @@ List.filter (Fun.negate string_empty) lines;;

