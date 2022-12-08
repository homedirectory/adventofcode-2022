#use "helpers.ml";;

(*let lines = file_to_lines "input.test";;*)
let lines = file_to_lines "input";;
let matrix = List.map (fun ln -> List.map atoi (str_explode ln)) lines;;

(* combine the same row traversed from different sides to determine
   final visiblity for trees *)
let merge_rows row1 row2 =
    (* height is the same for both, so choose whichever *)
    List.map2 (fun (h, vis1) (_, vis2) -> (h, vis1 || vis2)) row1 row2;;

let score height left right =
    let pred h = h >= height in
    List.length (stop_after pred (List.rev left)) 
    *
    List.length (stop_after pred right);;

(* replace heights with scores *)
let map_row row =
    let rec iter left tail =
        match tail with
        | [] -> []
        | h :: t -> (score h left t) :: (iter (left @ [h]) t)
    in
    iter [] row;;


(*let part2 () =*)
let from_rows = List.map map_row matrix;;
let from_cols = List.map map_row (transpose matrix);;
let matr = List.map2 (fun r1 r2 -> List.map2 mul r1 r2) from_rows (transpose from_cols);;
let max_score = list_max (List.flatten matr);;
