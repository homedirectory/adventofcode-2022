open Aoc.Helpers
open Printf

let print_cube (x, y, z) =
    printf "(%d,%d,%d)\n" x y z

(* a cube's side is represented as a pair of coordinates of its diagonal *)
let sides_of_cube (x, y, z) =
    [((x,y,z), (x+1,y,z+1)); 
    ((x,y+1,z), (x+1,y+1,z+1));
    ((x+1,y,z), (x+1,y+1,z+1)); 
    ((x,y,z), (x,y+1,z+1));
    ((x,y,z), (x+1,y+1,z)); 
    ((x,y,z+1), (x+1,y+1,z+1))]

(* filter a list by preserving only unique elements *)
let list_only_uniq cmpf lst =
    let rec iter prev acc = function
        | [] -> acc
        | hd :: tl ->
            if hd = prev
            then iter hd acc tl
            else
                match tl with
                | [] -> hd :: acc
                | tlhd :: tltl ->
                    if hd = tlhd
                    then iter tlhd acc tltl
                    else iter hd (hd :: acc) tl
    in
    match List.sort cmpf lst with
    | [] -> []
    | hd :: [] -> [hd]
    | a1 :: a2 :: tl ->
        if a1 = a2
        then iter a2 [] tl
        else iter a1 [a1] (a2 :: tl)

let parse_cubes text =
    let lines = String.split_on_char '\n' text |> List.filter (Fun.negate string_empty) in
    List.map (fun ln -> Scanf.sscanf ln "%d,%d,%d" (fun x y z -> (x, y, z))) lines

let surf_area cubes =
    let sides = List.map sides_of_cube cubes |> List.flatten in
    list_only_uniq compare sides |> List.length

(* part 1: map each cube to its sides, then count only unique ones *)
let part1 input =
    let cubes = parse_cubes input in
    surf_area cubes

type t = Cube | Trap | Free | Air

let cube_neighbs (x, y, z) =
    [
        (x-1, y, z); (x+1, y, z);
        (x, y-1, z); (x, y+1, z);
        (x, y, z-1); (x, y, z+1);
    ]

let pos3_to_str (x, y, z) =
    sprintf "(%d, %d, %d)" x y z

let normalize_cubes cubes =
    let x0 = list_min_map (fun (x, _, _) -> x) cubes in
    let y0 = list_min_map (fun (_, y, _) -> y) cubes in
    let z0 = list_min_map (fun (_, _, z) -> z) cubes in
    List.map (fun (x, y, z) -> (x - x0, y - y0, z - z0)) cubes

let mk_3d_arr v d1 d2 d3 =
    Array.init d1 (fun _ -> (Array.init d2 (fun _ -> (Array.make d3 v))))

let arr_3d_get arr (x, y, z) =
    arr.(x).(y).(z)

let arr_3d_get_opt arr pos =
    try Some (arr_3d_get arr pos) with _ -> None

let arr_3d_set arr v (x, y, z) =
    arr.(x).(y).(z) <- v

let arr_3d_set_all arr v pos_lst =
    List.iter (fun p -> arr_3d_set arr v p) pos_lst

let arr_3d_iteri f arr =
    Array.iteri (fun d1 arr2 -> 
        Array.iteri (fun d2 arr1 -> 
            Array.iteri (fun d3 v -> f (d1, d2, d3) v) arr1) arr2) arr

let arr_3d_foldl f init arr =
    Array.fold_left (fun acc arr2 -> 
        Array.fold_left (fun acc1 arr1 -> 
            Array.fold_left f acc1 arr1) acc arr2) init arr

let arr_3d_to_flatlist arr =
    List.rev (arr_3d_foldl (fun acc v -> v :: acc) [] arr)

let arr_3d_dims arr =
    let d1 = Array.length arr in
    let d2 = 
        if d1 = 0 then 0
        else Array.length arr.(0)
    in
    let d3 =
        if d2 = 0 then 0
        else Array.length arr.(0).(0)
    in
    (d1, d2, d3)

let arr_3d_is_edge arr (x, y, z) =
    let (d1, d2, d3) = arr_3d_dims arr in
    x = 0 || x = d1 - 1 ||
    y = 0 || y = d2 - 1 ||
    z = 0 || z = d3 - 1

let list_without excluded lst =
    List.filter (fun x -> not (List.mem x excluded)) lst

let adj_air arr pos =
    List.filter (fun p -> 
        match arr_3d_get_opt arr p with 
        | None -> false 
        | Some sm -> sm = Air
    ) (cube_neighbs pos)

let scan arr =
    let is_free pos =
        (arr_3d_get_opt arr pos) = Some Free
    in
    let is_out pos =
        (arr_3d_get_opt arr pos) = None
    in
    let is_cube_at pos =
        (arr_3d_get_opt arr pos) = Some Cube
    in
    let rec step acc =
        match List.map (cube_neighbs) acc |> List.flatten |> list_uniq |> filter_not is_cube_at |> list_without acc with
        | [] -> (false, acc)
        | adj -> 
            let next_acc = adj @ acc in
            match List.find_opt (fun p -> is_free p || is_out p) adj with
            | None -> step next_acc
            | Some _ -> (true, (filter_not is_out next_acc))
    in
    let n = ref 0 in
    arr_3d_iteri (fun pos v -> 
        match v with
        | Cube | Free | Trap -> ()
        | Air ->
            if arr_3d_is_edge arr pos
            then arr_3d_set arr Free pos
            else
                match step [pos] with
                | (false, path) -> arr_3d_set_all arr Trap path;
                    n := !n + (surf_area path)
                | (true, path) -> arr_3d_set_all arr Free path
    ) arr;
    !n

let count_trapped_sides cubes =
    let norm_cubes = normalize_cubes cubes in
    let x = list_max_map (fun (x, _, _) -> x) norm_cubes in
    let y = list_max_map (fun (_, y, _) -> y) norm_cubes in
    let z = list_max_map (fun (_, _, z) -> z) norm_cubes in
    let arr = mk_3d_arr Air (x+1) (y+1) (z+1) in
    List.iter (arr_3d_set arr Cube) norm_cubes;
    scan arr

(* part 2: count using part 1 solution, then substract those air cubes that are trapped *)
let part2 input =
    let p1 = part1 input in
    let cubes = parse_cubes input in
    let n = count_trapped_sides cubes in
    printf "Found %d trapped sides\n" n;
    p1 - n

let filename = input_for_day 18
let test_filename = test_input_for_day 18

let test_s1 = part1 (file_to_text test_filename)
let _ = printf "Part 1: %d\n" test_s1
let _ = assert (test_s1 = 64)

let s1 = part1 (file_to_text filename)
let _ = printf "Part 1: %d\n" s1
let _ = assert (s1 = 3396)

let test_s2 = part2 (file_to_text test_filename)
let _ = printf "Part 2: %d\n" test_s2
let _ = assert (test_s2 = 58)

let s2 = part2 (file_to_text filename)
let _ = printf "Part 2: %d\n" s2
let _ = assert (s2 = 2044)
