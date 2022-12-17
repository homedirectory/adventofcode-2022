open Printf;;
#use "helpers.ml";;

type pos = int * int

let pos_to_str (x,y) =
    sprintf "(%d,%d)" x y

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

let check_row row_y sensors all_posits =
    merge_ranges @@ list_only_some @@ List.map (coverage_of_row row_y) sensors

(* PARSING INPUT *)
let filename = "input"
let parse_line ln =
    Scanf.sscanf ln "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun sx sy bx by -> (sx,sy),(bx,by))
let sens_posits,beac_posits = List.split @@ List.map parse_line (file_to_lines filename)
let sensors = List.map2 (fun s b -> Sensor(s, Beacon(b))) sens_posits beac_posits
let all_posits = List.append sens_posits (list_uniq beac_posits)

let max_xy = 4000000

let find_uncovered (start,fin) (in_start,in_fin) =
    if start < in_start then Some start
    else if in_fin < fin then Some in_fin
    else None

let freq (x,y) =
    (x * 4000000) + y

let x_range = (0, max_xy+1);;

let finder y =
    let ranges = check_row y sensors all_posits in
    match List.find_map (find_uncovered x_range) ranges with
    | Some x -> Some (x,y)
    | None -> None

let distress_pos = match for_find finder 0 (max_xy+1) with
    | None -> failwith "No distress beacon found"
    | Some pos -> pos
    
let _ = printf "Distress beacon's signal coming from %s at frequency %s\n" 
    (pos_to_str distress_pos) (string_of_int (freq distress_pos))
