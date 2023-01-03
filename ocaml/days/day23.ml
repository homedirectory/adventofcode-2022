open Aoc.Helpers
open Printf

let list_cycle lst =
    list_sliding_window (List.length lst) (List.append lst (list_drop_right 1 lst))

type pos_t = int * int

let pos_to_str (x, y) =
    sprintf "(%d, %d)" x y

type tile_t = Elf | Ground

let tile_is_elf = function
    Elf -> true | _ -> false

let tile_of_char = function
    | '#' -> Elf
    | '.' -> Ground
    | c -> failwith (sprintf "tile_of_char: wtf %C" c)

type dir_t =
    | N | NE | NW
    | S | SE | SW
    | E
    | W

let dir_to_str = function
    | N -> "N"
    | S -> "S"
    | W -> "W"
    | E -> "E"

let adjacent_dirs = function
    | N -> [NE; NW]
    | S -> [SE; SW]
    | W -> [NW; SW]
    | E -> [NE; SE]

let dir_map_pos (x, y) = function
    | N -> (x, y - 1)
    | NE -> (x + 1, y - 1)
    | NW -> (x - 1, y - 1)
    | S -> (x, y + 1)
    | SE -> (x + 1, y + 1)
    | SW -> (x - 1, y + 1)
    | E -> (x + 1, y)
    | W -> (x - 1, y)

let neighbours pos =
    List.map (dir_map_pos pos) [NW; N; NE; W; E; SW; S; SE]

(* contains all elves keyed on their respective positions *)
type grid_t = (pos_t, unit) Hashtbl.t

let grid_count_elves grid =
    Hashtbl.length grid

let grid_get grid pos =
    Hashtbl.find_opt grid pos

let grid_is_elf_at grid pos =
    Option.is_some (grid_get grid pos)

let grid_is_ground_at grid pos = 
    not (grid_is_elf_at grid pos)

let grid_put grid pos item =
    Hashtbl.replace grid pos item;
    grid

let grid_of_positions lst =
    let grid = Hashtbl.create (List.length lst) in
    List.fold_left (fun acc pos -> grid_put acc pos ()) grid lst

let grid_move grid src_pos dst_pos =
    printf "move from %s to %s\n%!" (pos_to_str src_pos) (pos_to_str dst_pos);
    let grid = match grid_get grid src_pos with
        | Some data -> grid_put grid dst_pos data
        | None -> failwith "grid_move: nothing at src"
    in
    Hashtbl.remove grid src_pos;
    grid

let grid_positions grid =
    List.of_seq (Hashtbl.to_seq_keys grid)

let grid_fold_posits f init grid =
    Hashtbl.fold (fun k _ acc -> f acc k) grid init

let print_elves grid =
    let posits = grid_positions grid in
    let (min_x, max_x) = list_min_and_max (List.map fst posits) in
    let (min_y, max_y) = list_min_and_max (List.map snd posits) in
    let adjusted = List.map (fun (x,y) -> (x - min_x, y - min_y)) posits in
    let m = Array.make_matrix (max_y - min_y + 1) (max_x - min_x + 1) '.' in
    List.iter (fun (x, y) -> m.(y).(x) <- '#') adjusted;
    print_newline ();
    print_matrix print_char "" m;
    ()


let parse_input inp =
    let lines = Str.split (Str.regexp_string "\n") inp in
    let positions = List.map str_explode lines
        |> List.mapi (fun y chars -> List.mapi (fun x c -> ((x, y), tile_of_char c)) chars)
        |> List.flatten
        |> List.filter (function (_, tile) -> tile_is_elf tile)
        |> List.map fst
    in
    grid_of_positions positions


let fst_neighb grid pos =
    let posits = neighbours pos in
    List.find_map (grid_get grid) posits

let fst_valid_dir dirs grid pos =
    (*printf "fst_valid_dir %s\n" (pos_to_str pos);*)
    List.find_opt (fun dir ->
        let posits = List.map (dir_map_pos pos) (dir :: (adjacent_dirs dir)) in
        list_andmap (grid_is_ground_at grid) posits)
    dirs

(* the 1st half involves elves proposing their moves;
   returns a list of pairs (src, dst) *)
let round_half1 grid dirs =
    let tbl_dst_to_src = grid_fold_posits (fun acc pos ->
        match fst_neighb grid pos with
        | None -> 
            (*printf "%s does not do anything\n" (pos_to_str pos);*)
            acc
        | Some _ -> 
            match fst_valid_dir dirs grid pos with
            | None -> acc
            | Some dir ->
                let new_pos = dir_map_pos pos dir in
                tbl_map acc new_pos (function None -> Some pos | Some _ -> None);
                acc)
    (Hashtbl.create (grid_count_elves grid)) grid 
    in
    Hashtbl.fold (fun dst src acc -> 
        match src with 
        | None -> acc 
        | Some sm -> (sm, dst) :: acc)
    tbl_dst_to_src []

let round_half2 grid (moves: (pos_t * pos_t) list) =
    List.fold_left (fun acc (src, dst) -> grid_move acc src dst) grid moves

(* Part 1 single round *)
let round grid dirs =
    let grid_out = round_half2 grid (round_half1 grid dirs) in
    print_endline "End of round";
    print_list (fun dir -> print_string (dir_to_str dir)) dirs;
    print_elves grid_out;
    grid_out

let count_ground grid =
    let posits = grid_positions grid in
    let (min_x, max_x) = list_min_and_max (List.map fst posits) in
    let (min_y, max_y) = list_min_and_max (List.map snd posits) in
    let area = (max_x - min_x + 1) * (max_y - min_y + 1) in
    area - (List.length posits)

let part1 inp =
    let grid = parse_input inp in
    let dirs_sq = Seq.cycle (List.to_seq (list_cycle [N; S; W; E])) in
    let dirs_cycles = List.of_seq (Seq.take 10 dirs_sq) in
    let grid_out = List.fold_left round grid dirs_cycles in
    (grid_out, count_ground grid_out)

(* Part 2 single round *)
let round2 grid dirs =
    match round_half1 grid dirs with
    | [] -> None
    | moves -> Some (round_half2 grid moves)

let part2 inp =
    let grid = parse_input inp in
    let dirs_sq = Seq.cycle (List.to_seq (list_cycle [N; S; W; E])) in
    let ((grid_out, n), _) = seq_foldl_take (fun (grid_acc, n) dirs ->
        (*printf "Round %d\n%!" n;*)
        match round2 grid_acc dirs with
        | None -> None
        | Some grid_out -> Some (grid_out, n + 1))
    (grid, 0) dirs_sq
    in
    (grid_out, n + 1)

let test_input = file_to_text (test_input_for_day 23)
let (test_elves, test_s1) = part1 test_input
let _ = printf "Part 1 test: %d\n" test_s1

let input = file_to_text (input_for_day 23)
let (elves, s1) = part1 input
let _ = printf "Part 1: %d\n" s1

let test_input = file_to_text (test_input_for_day 23)
let (test_grid, test_s2) = part2 test_input
let _ = printf "Part 2 test: %d\n" test_s2

let input = file_to_text (input_for_day 23)
let (grid, s2) = part2 input
let _ = printf "Part 2: %d\n" s2

