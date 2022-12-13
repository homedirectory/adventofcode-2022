open Printf;;
#use "helpers.ml";;

type node = { c: char; mutable linked: node list; mutable visited: bool; pos: int * int };;

let add_link nd lnd =
    nd.linked <- lnd :: nd.linked;;

let rec node_copy nd =
    { c = nd.c; linked = List.map node_copy nd.linked; visited = nd.visited; pos = nd.pos};;

let elev = function
    | 'S' -> -1
    | 'E' -> 26
    | c -> Char.code c - 97;;

let rate src_node dst_node =
    let elev_dst, elev_src = (elev dst_node.c), (elev src_node.c) in
    let elev_r = if elev_dst > elev_src then 2 else if elev_dst = elev_src then 1 else 0 in
    let vis_r = if dst_node.visited then 0 else 2 in
    let rating = elev_r + vis_r in
    (* printf "rate src:%c dst:%c = %d\n%!" src_node.c dst_node.c rating; *)
    rating;;

let sort_rated =
    List.sort (fun (_, r1) (_, r2) -> (-1) * (Int.compare r1 r2));;

let rec top_rated lst =
    match lst with
    | [] -> []
    | (nd, r) :: [] -> [nd]
    | (nd1, r1) :: (nd2, r2) :: t -> 
            if r1 = r2 then nd1 :: (top_rated ((nd2, r2) :: t)) else [nd1];;

let next nd =
    top_rated @@ sort_rated @@ List.map (fun lnd -> lnd, rate nd lnd) nd.linked;;

let rec solve grid_printer curr_node path =
    (* Unix.sleepf 0.2; *)
    curr_node.visited <- true;
    (* grid_printer curr_node.pos; print_newline(); *)
    printf "%c %d\n%!" curr_node.c (List.length path);
    match curr_node.c with
    | 'E' -> path
    | _ -> 
        match next curr_node with
        | [] -> raise (Error "no next node")
        | h :: t -> solve grid_printer h (path @ [curr_node])
        (* branch off *)
        (*
        | lst -> List.(hd @@ sort Int.compare @@
            map (fun nd -> length @@ solve (node_copy nd) (path @ [curr_node])) lst);;
        *)
        ;;

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
    let nodes = matrix_mapi (fun pos c -> {c=c; linked=[]; visited=false; pos=pos}) grid in
    let neighbours (x, y) =
        List.map (fun (x,y) -> nodes.(y).(x)) @@
            List.filter (fun (xn, yn) -> xn >= 0 && xn < w && yn >= 0 && yn < h)
                [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
    in

    matrix_iteri (fun pos nd -> List.iter (add_link nd) @@ 
        List.filter (is_reachable nd) (neighbours pos))
        nodes;
    nodes.(0).(0);;

let parse lines =
    Array.(map (fun ln -> of_list @@ str_explode ln) @@ of_list lines);;
let grid = parse (file_to_lines "input");;
let graph = graph_of_grid grid;;

let print_grid pos =
    Array.(iteri (fun y row -> (iteri (fun x c -> 
            if pos = (x,y) then printf "# " else printf "%c " c) row); print_newline())
        grid);; 

let paths = solve print_grid graph [];;

(* print_matrix print_char grid;; *)
(* let end_pos = matrix_find (fun c -> c = 'E') grid;; *)
