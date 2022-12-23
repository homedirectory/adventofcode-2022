open Aoc.Helpers
open Printf

let filename = input_for_day 12

type node = { c: char; mutable linked: node list; pos: int * int; mutable visited: bool };;

let pos_x = function
    (x, y) -> x;;

let pos_y = function
    (x, y) -> y;;

let node_x node =
    pos_x node.pos;;

let node_y node =
    pos_y node.pos;;

let get_unvisited node =
    List.filter (fun nd -> not nd.visited) node.linked;;

let mark_visited nodes =
    List.iter (fun nd -> nd.visited <- true) nodes;
    nodes;;

let node_to_str { c=c; linked=_; pos=(x, y); visited=v } =
    sprintf "%c (%d, %d)" c x y;;

let print_node node =
    print_endline (node_to_str node);;

let add_link nd lnd =
    nd.linked <- lnd :: nd.linked;;

let elev = function
    | 'S' -> 0
    | 'E' -> 26
    | c -> Char.code c - 97;;

let is_end node =
    node.c = 'E';;

let print_graph nodes =
    let max_x = node_x List.(hd (list_sortf_desc node_x nodes)) in 
    let max_y = node_y List.(hd (list_sortf_desc node_y nodes)) in 
    let mtrx = Array.make_matrix (max_y+1) (max_x+1) '.' in
    List.iter (fun nd -> mtrx.(node_y nd).(node_x nd) <- nd.c) nodes;
    print_matrix print_char "" mtrx;;

let solve root =
    (* visited is used for print_graph *)
    let rec iter n visited nodes =
        (* Unix.sleepf 0.05; *)
        printf "iter %d\n" n;
        print_graph visited; print_newline();
        match List.find_opt (fun nd -> is_end nd) nodes with
        | None -> iter (n+1) (visited @ nodes) 
            (List.flatten (List.map (fun nd -> mark_visited (get_unvisited nd)) nodes))
        | Some _ -> n
    in
    iter 0 [root] (mark_visited [root]);;

let is_reachable src_node dst_node =
    match src_node.c, dst_node.c with
    | 'S', _ -> true
    | sc, dc -> (elev dc) - (elev sc) <= 1;;

let graph_of_grid grid =
    let h, w = matrix_dims grid in
    let nodes = matrix_mapi (fun pos c -> {c=c; linked=[]; pos=pos; visited=false}) grid in
    let neighbours (x, y) =
        List.map (fun (x,y) -> nodes.(y).(x)) @@
            List.filter (fun (xn, yn) -> xn >= 0 && xn < w && yn >= 0 && yn < h)
                [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
    in
    matrix_iteri (fun pos nd -> List.iter (add_link nd) @@
        List.filter (is_reachable nd) (neighbours pos)) nodes;
    match Array.find_opt (fun nd -> nd.c = 'S') (matrix_flatten nodes) with
    | None -> failwith "bad input: no start node"
    | Some nd -> nd;;

let parse lines =
    Array.(map (fun ln -> of_list @@ str_explode ln) @@ of_list lines);;

let grid = parse (file_to_lines filename);;
let graph = graph_of_grid grid;;

let len = solve graph;;
printf "Part 1: %d\n" len;;
