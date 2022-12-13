open Printf;;
#use "helpers.ml";;

type node = { c: char; mutable linked: node list; pos: int * int };;

let node_eq node1 node2 =
    node1.pos = node2.pos;;

let node_to_str { c=c; linked=_; pos=(x, y) } =
    sprintf "%c (%d, %d)" c x y;;

let print_node node =
    print_endline (node_to_str node);;

let add_link nd lnd =
    nd.linked <- lnd :: nd.linked;;

let elev = function
    | 'S' -> -1
    | 'E' -> 26
    | c -> Char.code c - 97;;

let rate src_node dst_node visited =
    let n = List.length visited in
    let elev_dst, elev_src = (elev dst_node.c), (elev src_node.c) in
    let elev_r = if elev_dst > elev_src then n else if elev_dst = elev_src then n/2 else 0 in
    let vis_r = match list_ind_optf node_eq dst_node (List.rev visited) with
        | None -> n
        | Some i -> i
    in
    let rating = elev_r + vis_r in
    (* printf "rate src:%s dst:%s = %d\n%!" (node_to_str src_node) (node_to_str dst_node) rating; *)
    rating;;

let sort_by_rating =
    List.sort (fun (_, r1) (_, r2) -> (-1) * (Int.compare r1 r2));;

let take_while_eqf eqf lst =
    let rec iter prev = function
        | [] -> []
        | h :: t -> if eqf h prev then h :: (iter h t) else []
    in
    match lst with
    | [] -> []
    | a :: t -> a :: (iter a t);;

let choose_next node visited =
    List.map (fun (nd, _) -> nd) @@ take_while_eqf (fun (_, r1) (_, r2) -> r1 = r2) @@ 
        sort_by_rating @@ List.map (fun nd -> nd, rate node nd visited) node.linked;;

let is_end node =
    node.c = 'E';;

let rec solve node path =
    (* begin with a starting node and a path of visited nodes
       if position is E -> return (path, forks)
       else determine next step:
           if multiple nodes have the same rating, choose randomly, save the rest for later (forks)
           else just advance *)
    let rec iter nd pth forks =
        print_node nd;
        (* Unix.sleepf 0.05; *)
        if is_end nd then (pth, forks)
        else match choose_next nd pth with
        | [] -> raise (Error "no next nodes")
        | h :: t -> 
            (* print_endline ">>> NEXT NODES >>>"; print_node h; List.iter print_node t; print_endline "<<< NEXT NODES <<<"; *)
            iter h (pth @ [h]) (forks @ (List.map (fun tnd -> tnd, pth) t))
    in

    match iter node (path @ [node]) [] with
    | pth, forks -> printf "Found path of length %d with %d forks\n" (List.length pth) (List.length forks);
        pth :: (List.flatten @@ List.map (fun (frk_node, frk_path) -> solve frk_node frk_path) forks);;
        
let is_reachable src_node dst_node =
    match src_node.c, dst_node.c with
    | 'S', _ -> true
    | 'E', _ -> true
    | _, 'S' -> true
    | 'z', 'E' -> true
    | _, 'E' -> false
    | sc, dc -> (elev dc) - (elev sc) <= 1;;

let graph_of_grid grid =
    let h, w = matrix_dims grid in
    let nodes = matrix_mapi (fun pos c -> {c=c; linked=[]; pos=pos}) grid in
    let neighbours (x, y) =
        List.map (fun (x,y) -> nodes.(y).(x)) @@
            List.filter (fun (xn, yn) -> xn >= 0 && xn < w && yn >= 0 && yn < h)
                [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
    in

    matrix_iteri (fun pos nd -> List.iter (add_link nd) @@
        List.filter (is_reachable nd) (neighbours pos))
        nodes;
    let start = nodes.(0).(0) in
    start;;

let parse lines =
    Array.(map (fun ln -> of_list @@ str_explode ln) @@ of_list lines);;
(* let grid = parse (file_to_lines "input.test");; *)
let grid = parse (file_to_lines "input");;
let graph = graph_of_grid grid;;

let print_grid pos =
    Array.(iteri (fun y row -> (iteri (fun x c ->
            if pos = (x,y) then printf "# " else printf "%c " c) row); print_newline())
        grid);;

let paths = solve graph [];;
List.iter (fun pth -> printf "%d: " (List.length pth); 
    print_list (fun node -> print_char node.c) pth) paths;;

(* print_matrix print_char grid;; *)
(* let end_pos = matrix_find (fun c -> c = 'E') grid;; *)
