open Printf;;
#use "helpers.ml";;

type value = { is_int: bool; n: int; lst: value list };;

let rec value_to_str { is_int; n; lst } =
    if is_int then string_of_int n
    else sprintf "[%s]" (String.concat "," @@ List.map value_to_str lst);;

let print_value v =
    print_endline (value_to_str v);;

let mk_value is_int n lst = {
    is_int = is_int;
    n = n;
    lst = lst
}

let mk_int n =
    mk_value true n [];;

let mk_list lst =
    mk_value false 0 lst;;

let convert v =
    if v.is_int then mk_list [v] else v;;

let rec find_map f = function
    | [], [] -> None
    | [], rh :: rt -> Some true
    | lh :: lt, [] -> Some false
    | lh :: lt, rh :: rt -> match f (lh, rh) with
        | None -> find_map f (lt, rt)
        | s -> s;;

let rec cmp pair = 
    match pair with
    | v1 :: v2 :: [] -> printf "cmp %s %s\n" (value_to_str v1) (value_to_str v2);
        match v1, v2 with
        | {is_int=true; n=n1; lst=_}, {is_int=true; n=n2; lst=_} -> 
                if n1 < n2 then Some true else if n1 > n2 then Some false else None
        | {is_int=false; n=_; lst=lst1}, {is_int=false; n=_; lst=lst2} ->
                find_map (fun (l1,l2) -> cmp [l1;l2]) (lst1, lst2)
        | v1, v2 -> if v1.is_int then cmp [(convert v1); v2] else cmp [v1; (convert v2)];;

(* TEST *)
let v1 = mk_list [mk_int 1; mk_int 1; mk_int 3; mk_int 1];;
let v2 = mk_list [mk_int 1; mk_int 1; mk_int 5; mk_int 1];;

let str_to_int s =
    Scanf.sscanf s "%d" Fun.id;;

let parse_int chars =
    let s = str_of_chars chars in
    let start = Str.search_forward (Str.regexp "[0-9]+") s 0 in
    let _end = Str.match_end() in
    str_to_int (String.sub s start _end), str_explode (str_from _end s);;

let parse str =
    let rec parse_chars k = function
        | [] -> k [] []
        | '[' :: t -> parse_chars (fun els tail -> 
                parse_chars (fun kels ktail -> k (mk_list els :: kels) ktail) tail) t
        | ']' :: t -> k [] t
        | ',' :: t -> parse_chars k t
        | d :: t -> let (i, rest_chars) = parse_int (d::t) in
            parse_chars (fun kels ktail -> k (mk_int i :: kels) ktail) rest_chars
    in
    parse_chars (fun els tail -> List.nth els 0) (str_explode str);;

let packets = List.map parse @@ List.filter (Fun.negate string_empty) @@ 
    file_to_lines "input";;

let part1 () = 
    let pairs = list_windowed 2 @@ packets in
    let part1 = list_sum @@ List.mapi 
        (fun i p -> match cmp p with None -> raise (Error "cmp returned None") 
                | Some true -> i + 1 | Some false -> 0) 
        pairs in

    printf "Part 1: %d\n" part1;;

(* PART 2 *)
let part2 () =
    let div2 = (mk_list [mk_list [mk_int 2]]) in
    let div6 = (mk_list [mk_list [mk_int 6]]) in
    let sorted = List.sort (fun p1 p2 -> match cmp [p1; p2] with
        | None -> 0
        | Some true -> -1
        | Some false -> 1) 
        (div2 :: div6 :: packets) in

    let div2_ind = 1 + (Option.get @@ list_ind_optf (=) div2 sorted) in
    let div6_ind = 1 + (Option.get @@ list_ind_optf (=) div6 sorted) in
    let key = div2_ind * div6_ind in
    printf "Part 2: %d\n" key;;

part2();;
