#use "helpers.ml";;

type 'a stack = { mutable items: 'a list };;
let stack_init items = {items=items};;

let print_list lst =
    List.iter (Printf.printf "%c ") lst;
    print_endline "";;
let print_stack {items} = 
    print_list items;;

let rec peek n {items} =
    match items with 
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: peek (n-1) {items};;

let pop n st = 
    if n > List.length st.items then raise Out_of_bounds;
    let took, left = split_before n st.items in
    st.items <- left;
    took;;

let push items st =
    (* yes, this is hacky, im tired and want to sleep *)
    (* Part 1 *)
    (*st.items <- (List.rev items) @ st.items;*)
    (* Part 2 *)
    st.items <- items @ st.items;
    st;;

let move_from_to n src dst = 
    push (pop n src) dst;;

type move = {n : int; src: int; dst: int}
let move_init n src dst = {n=n; src=src; dst=dst};;

let do_move stacks {n; src; dst} = 
    List.iteri (fun i st -> Printf.printf "%d: " (i+1); print_stack st) stacks;
    Printf.printf "move %d from %d to %d\n" n (src+1) (dst+1);
    ignore (move_from_to n (List.nth stacks src) (List.nth stacks dst));
    ();;

let lines = file_to_lines "input";;
let stacks_lines, moves_lines = 
    match (split_after 7 lines) with
    left, right -> left, drop 2 right;;

let stacks = List.map stack_init @@ List.map (List.filter (fun c -> c != ' ')) @@ transpose @@ List.map (fun ln -> list_every_nth 4 (str_explode (str_from 1 ln))) stacks_lines;;

let moves = List.map (fun ln -> Scanf.sscanf ln "move %d from %d to %d" 
                     (fun n fr t -> move_init n (fr-1) (t-1)))
    moves_lines;;

List.iteri (fun i st -> Printf.printf "%d: " (i+1); print_stack st) stacks;;

let part1 () = 
    List.iter (do_move stacks) moves;
    List.iteri (fun i st -> Printf.printf "%d: " (i+1); print_stack st) stacks;
    let ans1 = List.fold_left (^) "" @@ List.map (String.make 1) @@ List.map List.hd @@ List.filter (Fun.negate list_empty) @@ List.map (peek 1) stacks in
    Printf.printf "Part 1: %s\n" ans1;;

part1 ();;
