exception Empty_list of string;;

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


let list_sort_desc lst = List.sort (fun x y -> (compare x y) * -1) lst;;
let list_sort_asc lst = List.sort compare lst;;

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
