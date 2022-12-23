open Aoc.Helpers
open Printf

let filename = input_for_day 15

type pos = int * int

type beacon = 
    | Beacon of pos

type sensor =
    | Sensor of pos * beacon

let distance (x1,y1) (x2,y2) =
    (Int.abs (x1-x2)) + (Int.abs (y1-y2))

let sensor_pos = function
    | Sensor (pos,_) -> pos

let dist_to_beac = function
    | Sensor (spos, Beacon bpos) -> distance spos bpos

let coverage_of_row y sensor =
    let db = dist_to_beac sensor in
    let (sx,sy) = (sensor_pos sensor) in
    let dy = distance (sx,sy) (sx,y) in 
    if dy > db then None
    else 
        match db - dy with
        | 0 -> Some(sx, sx + 1)
        | d -> Some((sx - d), (sx + d + 1))

let merge_ranges lst =
    let merge (s1,e1) (s2,e2) =
        if s2 <= e1
        then if e2 <= e1 then [(s1,e1)] else [(s1,e2)]
        else [(s1,e1);(s2,e2)]
    in
    let rec iter curr acc = function
        | [] -> curr :: acc 
        | h :: t -> match merge curr h with
            | [r] -> iter r acc t
            | [r1;r2] -> iter r2 (r1 :: acc) t
            | _ -> assert false
    in
    let sorted = List.sort (fun (start1,_) (start2,_) -> Int.compare start1 start2) lst in
    match sorted with
    | [] -> []
    | h :: t -> List.rev @@ iter h [] t

let make_ranges lst =
    list_windowmap (function [a;b] -> (a,b) | _ -> assert false) 2 lst

let range_splitif f (a,b) =
    let rec iter i acc =
        if i >= b then acc
        else if (f i) then iter (i+1) (i :: acc) else iter (i+1) acc
    in
    if a = b then []
    else
        let split_points = List.rev @@ iter (a+1) [] in
        match split_points with
        | [] -> [(a,b)]
        | h :: [] -> [(a,h);(h,b)]
        | lst -> make_ranges ((a :: lst) @ [b])

(* 
   split (1, 10) on [3;6] 
   (1,3); (4,6); (7,10)
   split (1, 10) on [3;10] 
   (1,3); (4;10)
   split (1, 10) on [1;10] 
   (2, 10)
*)
let range_split p (start,fin) =
    if start = fin then failwith "an empty range can't be split"
    else if p = start then [(start+1,fin)]
    else if p = fin then [(start,fin)]
    else if p > start && p < fin then [(start,p);(p+1,fin)]
    else [(start,fin)]

let range_splits points (start,fin) =
    let rec iter r r_lst = function
        | [] -> r :: r_lst
        | h :: t -> match range_split h r with
            | [r1] -> iter r1 r_lst t
            | [r1;r2] -> iter r2 (r1 :: r_lst) t
            | _ -> assert false
    in
    let sorted = List.sort Int.compare points in
    List.rev @@ iter (start,fin) [] sorted 

let range_len (start, fin) =
    fin - start

let check_row row_y sens_posits beac_posits =
    let sensors = List.map2 (fun s b -> Sensor(s, Beacon(b))) sens_posits beac_posits in
    let ranges = merge_ranges @@ list_only_some @@ List.map (coverage_of_row row_y) sensors in
    let all_posits = List.append sens_posits beac_posits in
    let all_x = List.map (fun (x,_) -> x) @@ List.filter (fun (_,y) -> y = row_y) all_posits in
    printf "before filtering\n%!";
    List.flatten @@ List.map (range_splits all_x) ranges

(* PARSING INPUT *)
let parse_line ln =
    Scanf.sscanf ln "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun sx sy bx by -> (sx,sy),(bx,by))
let sens_posits,beac_posits = List.split @@ List.map parse_line (file_to_lines filename)

(* let y = 10 *)
let y = 2000000
let ranges = check_row y sens_posits beac_posits
let n = list_sum @@ List.map range_len ranges
let _ = printf "Part 1: %d\n" n
