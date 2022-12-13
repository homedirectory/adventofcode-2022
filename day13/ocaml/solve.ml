open Printf;;
#use "helpers.ml";;

type value =
    | Int of int
    | List of value list;;

let rec value_to_str = function
    | Int i -> string_of_int i
    | List lst -> sprintf "[%s]" (String.concat "," @@ List.map value_to_str lst);;

let print_value v =
    print_endline (value_to_str v);;

let convert v =
    match v with
    | Int i -> List [v]
    | v -> v;;

let rec find_map f = function
    | [], [] -> None
    | [], rh :: rt -> Some true
    | lh :: lt, [] -> Some false
    | lh :: lt, rh :: rt -> match f (lh, rh) with
        | None -> find_map f (lt, rt)
        | s -> s;;

let rec cmp (v1, v2) =
    (* printf "cmp %s %s\n" (value_to_str v1) (value_to_str v2); *)
    match v1, v2 with
    | Int i1, Int i2 -> if i1 = i2 then None else Some (i1 < i2)
    | List lst1, List lst2 -> find_map cmp (lst1, lst2)
    | v1, v2 -> cmp (convert v1, convert v2);;

let parse_int chars =
    let str = str_of_chars chars in
    Scanf.sscanf str "%d%s" (fun i s -> i, (str_explode s));;

let parse str =
    let rec parse_chars k = function
        | [] -> k [] []
        | '[' :: t -> parse_chars (fun vals tail ->
                parse_chars (fun kvals ktail -> k (List vals :: kvals) ktail) tail) t
        | ']' :: t -> k [] t
        | ',' :: t -> parse_chars k t
        | chars -> let (i, rest_chars) = parse_int chars in
            parse_chars (fun kvals ktail -> k (Int i :: kvals) ktail) rest_chars
    in
    parse_chars (fun vals tail -> List.hd vals) (str_explode str);;

let packets = List.map parse @@ List.filter (Fun.negate string_empty) @@
    file_to_lines "input";;

let part1 () =
    let pairs = List.map list_to_pair @@ list_windowed 2 @@ packets in
    let part1 = list_sum @@ List.mapi (fun i p ->
        match cmp p with
        | None -> failwith "cmp returned None"
        | Some true -> i + 1 
        | Some false -> 0)
    pairs in
    printf "Part 1: %d\n" part1;;

part1();;

(* PART 2 *)
let part2 () =
    let div2 = List [List [Int 2]] in
    let div6 = List [List [Int 6]] in

    let sorted = List.sort (fun p1 p2 ->
        match cmp (p1, p2) with
        | None -> 0 | Some true -> -1 | Some false -> 1)
    (div2 :: div6 :: packets) in

    let div2_ind = 1 + (list_ind div2 sorted) in
    let div6_ind = 1 + (list_ind div6 sorted) in
    let key = div2_ind * div6_ind in
    printf "Part 2: %d\n" key;;

part2();;
