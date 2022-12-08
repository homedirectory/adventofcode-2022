#use "helpers.ml";;

(*let lines = file_to_lines "input.test";;*)
let lines = file_to_lines "input";;
let matrix = List.map (fun ln -> List.map atoi (str_explode ln)) lines;;

(* combine the same row traversed from different sides to determine
   final visiblity for trees *)
let merge_rows row1 row2 =
    (* height is the same for both, so choose whichever *)
    List.map2 (fun (h, vis1) (_, vis2) -> (h, vis1 || vis2)) row1 row2;;

(* replace heights with tuples (height, visible?) *)
let map_row row =
    (* first and last are always visible *)
    let rec iter max_h tail =
        match tail with
        | [] -> []
        | h :: [] -> []
        | h :: t -> let vis = h > max_h in
            Printf.printf "%d %b\n%!" h vis;
            let new_max_h = if vis then h else max_h in
            (h, vis) :: (iter new_max_h t)
    in
    (* 1. go from 0 to n-1 *)
    let ltr = iter (List.hd row) (List.tl row) in
    (* 2. reverse *)
    let rev = List.rev row in
    let rtl = iter (List.hd rev) (List.tl rev) in

    merge_rows ltr (List.rev rtl);;

let part1 () =
    (* ignore first and last row *)
    let from_rows = List.map map_row (list_strip_n 1 matrix) in
    (* ignore first and last col *)
    let from_cols = List.map map_row (list_strip_n 1 (transpose matrix)) in
    let interior = List.map2 merge_rows from_rows (transpose from_cols) in
    let inner_visible_n = List.fold_left (fun u (_, vis) -> u + (int_of_bool vis)) 0 @@
        List.flatten inner in
    (* all outer trees are visible *)
    let edge_total = 4 + ((List.length matrix - 2) * 4) in
    let total = edge_total + inner_visible_n in

    Printf.printf "Part 1: %d\n" total;;

part1();;

let map_score row =

let part2 () =
    let from_rows = List.map map_row matrix in
    let from_cols = List.map map_row (transpose matrix) in
    let matr = List.map2 merge_rows from_rows (transpose from_cols) in


