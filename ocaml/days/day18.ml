open Aoc.Helpers
open Printf

type pos_t = int * int * int

let pos_to_str (x, y, z) =
    sprintf "(%d, %d, %d)" x y z

let print_pos pos =
    print_string (pos_to_str pos)

let pos_x (x, _, _) = x
let pos_y (_, y, _) = y
let pos_z (_, _, z) = z

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

type side_t = Top | Bot | Right | Left | Front | Back

let list_adjacent (x, y, z) =
    [
        ((x - 1, y, z), Right); ((x + 1, y, z), Left);
        ((x, y - 1, z), Back); ((x, y + 1, z), Front);
        ((x, y, z - 1), Top); ((x, y, z + 1), Bot);
    ]

let list_without excluded lst =
    List.filter (fun x -> not (List.mem x excluded)) lst

let list_append_dedup lst1 lst2 =
    List.append lst1 lst2 |> list_uniq

let list_min_and_max_map minf maxf = function
    | [] -> failwith "list_min_and_max: empty list"
    | hd :: tl ->
        List.fold_left (fun (minv, maxv) a -> (min minv (minf a), max maxv (maxf a)))
        (minf hd, maxf hd) tl

let tbl_of_keys f keys =
    List.map (fun k -> (k, f k)) keys |> List.to_seq |> Hashtbl.of_seq

(* TODO replace list by set *)
let trace_exterior init pred nextf =
    let rec iter seen next acc =
        match list_flatmap nextf next |> list_uniq |> list_without seen with
        | [] -> acc
        | lst ->
            let (partt, partf) = List.partition pred lst in
            iter (list_append_dedup next seen) partt (list_append_dedup partf acc)
    in
    iter [init] [init] []

let part2 input =
    let cubes = parse_cubes input in
    let xmin, xmax = list_min_and_max_map pos_x pos_x cubes in
    let ymin, ymax = list_min_and_max_map pos_y pos_y cubes in
    let zmin, zmax = list_min_and_max_map pos_z pos_z cubes in

    let is_in_bounds (x, y, z) =
        x >= (xmin - 1) && x <= (xmax + 1) &&
        y >= (ymin - 1) && y <= (ymax + 1) &&
        z >= (zmin - 1) && z <= (zmax + 1)
    in

    let nextf pos =
        list_adjacent pos |> List.filter (fun (pos, _) -> is_in_bounds pos)
    in

    let cube_tbl = tbl_of_keys (fun _ -> ()) cubes in
    let is_air pos =
        not (Hashtbl.mem cube_tbl pos)
    in

    let start = ((xmin - 1, ymin - 1, zmin - 1), Top) in
    let ext = trace_exterior start (fun (pos, _) -> is_air pos) (fun (pos, _) -> nextf pos) 
        |> List.map fst in
    print_list print_pos ext;
    List.length ext

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
