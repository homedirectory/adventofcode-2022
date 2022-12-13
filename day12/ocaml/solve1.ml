open Printf;;
#use "helpers.ml";;

(* returns a list of visited positions *)
let solve grid end_pos =
    let h, w = Array.(length grid, length grid.(0)) in
    (* matrix of visiting order *)
    let vo_grid = Array.make_matrix h w (-1) in
    let vo (x, y) =
        vo_grid.(y).(x)
    in
    let set_vo (x, y) n =
        vo_grid.(y).(x) <- n
    in

    let get (x, y) =
        (* printf "get (%d, %d)\n%!" x y; *)
        grid.(y).(x)
    in

    let pos_to_str (x, y) =
        sprintf "[(%d, %d); %c]" x y (get (x,y))
    in

    let print_grid curr_pos =
        print_matrixi (fun (x,y) el -> 
            if (x,y) = curr_pos then print_char '#' else print_char el)
        grid
    in

    let neighbours (x, y) =
        List.filter (fun (xn, yn) -> xn >= 0 && xn < w && yn >= 0 && yn < h)
        [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
    in

    let elev pos =
        match get pos with
        | 'S' -> -1
        | 'E' -> 26
        | c -> (Char.code c) - 97
    in

    let elev_diff src dst =
        (elev dst) - (elev src)
    in

    let sort_positions pos lst =
        (* print_matrix (printf "%d") vo_grid; *)
        List.sort (fun p1 p2 -> let d1, d2 = elev_diff pos p1, elev_diff pos p2 in
            if d1 > d2 then -1
            else if d1 < d2 then 1
            else Int.compare (vo p1) (vo p2))
        lst
    in

    let pos_eq pos p1 p2 =
        elev_diff pos p1 = elev_diff pos p2 && vo p1 = vo p2
    in

    let is_reachable src (dx, dy) =
        (* printf "is_reachable %s %s\n" (pos_to_str src) (pos_to_str (dx,dy)); *)
        let diff = elev_diff src (dx, dy) in
        (* printf "diff=%d\n" diff; *)
        diff <= 1
    in

    let next pos =
        match List.filter (is_reachable pos) (neighbours pos) with
        | [] -> raise (Error ("no reachable positions from " ^ (pos_to_str pos)))
        | l -> 
            (*
            printf "Reachable: %s\n" (List.fold_left 
            (fun acc pos -> acc ^ ";" ^ (pos_to_str pos)) "" l); 
            *)
            (* prioritize next elevation *)
            List.hd (sort_positions pos l)
    in
   
    (* returns the next position *)
    let rec run i curr_pos visited branch_steps =
        Unix.sleepf 0.15;
        printf "step %d: %s\n%!" i (pos_to_str curr_pos);
        print_grid curr_pos;
        set_vo curr_pos i;
        match get curr_pos with
        | 'E' -> visited, branch_steps
        | _ -> let next_pos = next curr_pos in
            step (i+1) next_pos (visited @ [next_pos]) branch_steps
    in

    list_unfoldi step end_pos;;


let parse lines =
    Array.(map (fun ln -> of_list @@ str_explode ln) @@ of_list lines);;
let grid = parse (file_to_lines "input.test");;
print_matrix print_char grid;;
(* let end_pos = matrix_find (fun c -> c = 'E') grid;; *)
let start_pos = (0, 0);;
let visited = solve grid start_pos;;
