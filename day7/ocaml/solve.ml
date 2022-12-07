#use "helpers.ml";;

type file =
    | File of int * string
    | Dir of string;;

let file_to_str _file =
    match _file with
    | File (sz, name) -> Printf.sprintf "%d %s" sz name
    | Dir name -> Printf.sprintf "dir %s" name;;

let print_file _file =
    print_endline (file_to_str _file);;

type 'a tree = { parent: 'a tree option; value: 'a; mutable children: 'a tree list};;

let get_root node =
    let rec iter nd =
        match nd.parent with
        | None -> nd
        | Some p -> iter p
    in iter node;;

let print_tree val_printer tr =
    let rec iter indent {parent;value;children} = 
        match parent with
        | None -> ()
        | Some p -> 
            Printf.printf "%s" (String.make indent ' ');
            Printf.printf "parent: "; val_printer p.value;
            Printf.printf " ; ";
            Printf.printf "node: "; val_printer value;
            print_newline();
            List.iter (iter (indent+2)) children
            (*print_newline ()*)
    in iter 0 tr;;

let add_child value tr =
    let node = {parent = Some tr; value = value; children = []} in 
    tr.children <- node :: tr.children;
    node;;

let find_child pred tr =
    List.find pred tr.children;;

let find_child_by_filename name tr =
    Printf.printf "find_child_by_filename: %s\n%!" name;
    find_child (fun child -> match child.value with
        | File (_, s) -> s = name
        | Dir s -> s = name)
    tr;;

let tree_map f tr =
    let rec iter par curr =
        let new_node = { parent = par; value = (f curr); children = [] } in
        new_node.children <- List.map (iter (Some new_node)) curr.children;
        new_node
    in iter None tr;;

let rec tree_to_list tr =
    match tr.children with
    | [] -> [tr.value]
    | lst -> tr.value :: (List.concat @@ List.map tree_to_list lst);;

type token = 
    | Cd of string
    | Entry of file;;

let print_token tok =
    match tok with
    | Cd s -> Printf.printf "Cd %s\n%!" s
    | Entry _file -> Printf.printf "Entry "; print_file _file;;

let print_filetree tr =
    print_tree (fun value -> print_file value) tr;;

(*| Entry (Dir s) -> ?*)
(*| Entry (File (sz, s)) -> iter ? (add_child node f) t*)

let make_file_root () = 
    {parent = None; value = Dir "ROOT"; children = []};;

let build_tree tokens = 
    let rec iter cwd_node node tail =
        print_endline "*** build_tree:iter ***";
        print_filetree node;
        match tail with
        | [] -> get_root node
        | h::t -> print_token h; (match h with
            | Cd ".." -> (match cwd_node.parent with 
                | None -> raise (Error "node.parent is None")
                | Some nd -> iter nd nd t)
            | Cd s -> let new_cwd_node = (find_child_by_filename s cwd_node) in
                iter new_cwd_node new_cwd_node t
            | Entry f -> iter cwd_node (add_child f cwd_node) t
        )
    in let root = (make_file_root()) in
    ignore @@ add_child (Dir "/") root;
    iter root root tokens;;

let tokenize lines =
    List.map (fun ln ->
        match (String.split_on_char ' ' ln) with
        | "$"::"cd"::s::[] -> Cd s
        | "dir"::s::[] -> Entry (Dir s)
        | sz::s::[] -> Entry (File (int_of_string sz, s))
        | _ -> raise (Error "bad token")
    ) lines;;

let lines = filter_not (String.starts_with ~prefix:"$ ls") @@ file_to_lines "input";;
let tokens = tokenize lines;;
let tr = build_tree tokens;;
let root = List.hd tr.children;;

let rec get_size tr = 
    match tr.value with
    | File (sz, name) -> sz
    | Dir _ -> list_sum (List.map get_size tr.children);;

let files_and_sizes = tree_to_list @@ tree_map (fun node -> (node.value, get_size node)) root;; 
let dirs_and_sizes = List.filter (fun (_file, size) -> match _file with
    | File _ -> false
    | Dir _ -> true
) files_and_sizes;;

let part1 () =
    let limit = 100000 in
    let big_dirs = List.filter (fun (_, size) -> size <= limit) dirs_and_sizes in
    let ans = list_sum @@ List.map (fun (_, size) -> size) big_dirs in
    Printf.printf "Part 1: %d\n" ans;;

part1 ();;

let part2 () =
    let needed = 30000000 in
    let unused = 70000000 - (get_size root) in
    let to_free = needed - unused in

    let big_enough = List.map (fun (_, size) -> size) @@ 
        List.filter (fun (_, size) -> size >= to_free) dirs_and_sizes in
    let ans = list_min big_enough in

    Printf.printf "Part 2: %d\n" ans;;

part2 ();;
