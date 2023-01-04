open Aoc.Helpers
open Printf

type pos_t = int * int

let str_split_at_str sep str =
    Str.split (Str.regexp_string sep) str

let modulus x y =
    if x >= 0 then x mod y
    else y - ((x * (-1)) mod y)

let arr_drop_after x arr =
    match arr_findi_opt ((=) x) arr with
    | -1 -> arr
    | idx -> Array.sub arr 0 idx

let arr_drop_while pred arr =
    match arr_findi_opt (Fun.negate pred) arr with
    | -1 -> [| |]
    | idx -> arr_drop idx arr

let matrix_of_2dlist lst =
    List.map Array.of_list lst |> Array.of_list

(* align each string in a list to the same length with char c *)
let align_strings c lst =
    let max_len = list_max_map String.length lst in
    List.map (fun s -> s ^ (String.make (max_len - String.length s) c)) lst

type ins_t =
    | Step of int
    | R | L

let ins_of_char = function
    | 'R' -> R
    | 'L' -> L
    | c -> failwith (sprintf "unknown char %C" c)

module Board = struct
    type cell_t =
        | Tile
        | Wall

    let cell_of_char = function
        | '.' -> Tile
        | '#' -> Wall
        | c -> failwith (sprintf "wtf: %C" c)

    let print_cell = function
        | Tile -> print_char '.'
        | Wall -> print_char '#'

    type line_t = { start: int; cells: cell_t array }

    let mk_line start cells = 
        { start; cells }

    type t = { rows: line_t array; cols: line_t array }

    let make rows cols =
        { rows; cols }

    type dir_t = Up | Down | Right | Left

    let pp_dir ff = function
        | Up -> Format.fprintf ff "Up"
        | Down -> Format.fprintf ff "Down"
        | Left -> Format.fprintf ff "Left"
        | Right -> Format.fprintf ff "Right"

    let dir_map dir ins =
        match dir,ins with
        | Up, R -> Right
        | Right, R -> Down
        | Down, R -> Left
        | Left, R -> Up
        | Up, L -> Left
        | Right, L -> Up
        | Down, L -> Right
        | Left, L -> Down
        | _, Step _ -> failwith "wtf"

    let facing = function
        | Right -> 0
        | Down -> 1
        | Left -> 2
        | Up -> 3

    let get_col n brd =
        brd.cols.(n)

    let get_row n brd =
        brd.rows.(n)

    let move brd (x, y) n dir =
        Format.printf "move brd (%d, %d) %d %a\n" x y n pp_dir dir;
        let find_wall_bwd cells i =
            printf "find_wall_bwd %d\n" i;
            let j = i - n in
            let len = Array.length cells in
            let cell_path = begin
                if n >= (len - 1)
                then Array.append (arr_rev (arr_prefix cells i)) (arr_rev (arr_suffix cells (len - i - 1)))
                else if j >= 0 then arr_rev (arr_slice cells j i)
                else Array.append (arr_rev (arr_prefix cells i)) (arr_rev (arr_suffix cells (j * (-1))))
            end in
            print_array print_cell cell_path;
            arr_findi_opt (function Wall -> true | Tile -> false) cell_path
        in
        let find_wall_fwd cells i =
            printf "find_wall_fwd %d\n" i;
            let j = i + n in
            let len = Array.length cells in
            let cell_path = begin
                if n >= (len - 1)
                then Array.append (arr_suffix cells (len - i - 1)) (arr_prefix cells i)
                else if j < len then arr_slice cells (i + 1) (j + 1)
                else Array.append (arr_suffix cells (len - i - 1)) (arr_prefix cells (j - len + 1))
            end in
            arr_findi_opt (function Wall -> true | Tile -> false) cell_path
        in
        let move_bwd line c =
            let cc = c - line.start in
            let len = Array.length line.cells in
            let cc_out = match find_wall_bwd line.cells cc with
                | -1 -> modulus (cc - n) len
                | wc -> modulus (cc - wc) len
            in
            printf "%d + %d\n" cc_out line.start;
            cc_out + line.start
        in
        let move_fwd line c =
            let cc = c - line.start in
            let len = Array.length line.cells in
            let cc_out = match find_wall_fwd line.cells cc with
                | -1 -> modulus (cc + n) len
                | wc -> modulus (cc + wc) len
            in
            printf "%d + %d\n" cc_out line.start;
            cc_out + line.start
        in
        let (x1, y1) = match dir with
            | Up -> (x, move_bwd (get_col x brd) y)
            | Down -> (x, move_fwd (get_col x brd) y)
            | Right -> (move_fwd (get_row y brd) x, y)
            | Left -> (move_bwd (get_row y brd) x, y)
        in
        (x1, y1)

        (*
    let pp_board ff brd =
        let pp_rows ffr = Array.iter (pp_row ffr)
        Format.fprintf ff "@[board = %a@]@." pp_rows brd.rows
        *)
end

(* PARSING INPUT *)

let parse_input input =
    let parse_path path =
        let read_char str =
            Scanf.sscanf str "%c%s" (fun c s -> ((ins_of_char c), s))
        in
        let read_int str =
            Scanf.sscanf str "%d%s" (fun d s -> (Step d, s))
        in
        let read_any str =
            try read_char str with
            | _ -> try read_int str with
            | _ -> failwith (sprintf "failed to read next instruction %s" str)
        in
        Seq.unfold (function "" -> None 
            | s -> let (ins, ss) = read_any s in Some (ins, ss))
        path |> List.of_seq
    in
    let parse_board brd =
        let row_of_chars chars =
            let (ws, suf) = split_beforep ((<>) ' ') (Array.to_list chars) in
            let start = List.length ws in
            let (content, _) = split_beforep ((=) ' ') suf in
            Board.mk_line start (List.map Board.cell_of_char content |> Array.of_list)
        in
        let lines = str_split_at_str "\n" brd |> align_strings ' ' in
        let char_mtrx = List.map str_explode lines |> matrix_of_2dlist in
        let rows = matrix_rowmap row_of_chars char_mtrx in
        let cols = matrix_rowmap row_of_chars (matrix_transp char_mtrx) in
        Board.make rows cols
    in
    let [board; path] = str_split_at_str "\n\n" input in
    (parse_board board, parse_path path)

(* DONE PARSING INPUT *)

let password (x, y) dir =
    ((y + 1) * 1000) + ((x + 1) * 4) + (Board.facing dir)

let part1 input start =
    let (board, path) = parse_input input in
    let f (pos, dir) ins =
        match ins with
        | Step n -> 
            let new_pos = Board.move board pos n dir in
            (new_pos, dir)
        | R | L -> 
            let new_dir = Board.dir_map dir ins in
            (pos, new_dir)
    in
    let (pos, dir) = List.fold_left f (start, Right) path in
    password pos dir

module Cube = struct
    type face_t = {
        cells: Board.cell_t array;
        tr: pos_t -> pos_t
    }

    type t = {
        top: face_t; bot: face_t;
        left: face_t; right: face_t;
        front: face_t; back: face_t
    }
end

let parse_input2 input =
    let parse_path path =
        let read_char str =
            Scanf.sscanf str "%c%s" (fun c s -> ((ins_of_char c), s))
        in
        let read_int str =
            Scanf.sscanf str "%d%s" (fun d s -> (Step d, s))
        in
        let read_any str =
            try read_char str with
            | _ -> try read_int str with
            | _ -> failwith (sprintf "failed to read next instruction %s" str)
        in
        Seq.unfold (function "" -> None 
            | s -> let (ins, ss) = read_any s in Some (ins, ss))
        path |> List.of_seq
    in
    let parse_cube inp =
        let row_of_chars chars =
            let (ws, suf) = split_beforep ((<>) ' ') (Array.to_list chars) in
            let start = List.length ws in
            let (content, _) = split_beforep ((=) ' ') suf in
            List.map Board.cell_of_char content |> Array.of_list
        in
        let lines = str_split_at_str "\n" brd |> align_strings ' ' in
        let char_mtrx = List.map str_explode lines |> matrix_of_2dlist in
        let rows = matrix_rowmap row_of_chars char_mtrx in
        let n = 50 in
        let take_region (x, y) =
            let xn, yn = x + n, y + n in
            let row_slice = arr_slice rows y yn in
            if xn < Array.length row_slice.(0)
            then Array.map (fun row -> arr_slice row x xn) row_slice
            else if xn = Array.length row_slice.(0)
            then row_slice
            else failwith "xn >= 1st row in a slice"
        in
        let [back; right; bot; left; front; top] = List.map take_region 
            [(0,0); (50,0); (0,50); (0,100); (50,100); (0,150)]
        in
        Cube.of_faces left right top bot front back
    in
    let [tiles; path] = str_split_at_str "\n\n" input in
    (parse_cube tiles, parse_path path)

let part2 input start =
    let (board, path) = parse_input input in
    let cube = Cube.of_board board in
    let (pos, dir) = Cube.follow_path path start in
    password pos dir

let test_input = file_to_text (test_input_for_day 22)
let input = file_to_text (input_for_day 22)

let test_s1 = part1 test_input (8, 0)
let _ = printf "Part 1 test: %d\n" test_s1
let s1 = part1 input (50, 0)
let _ = printf "Part 1: %d\n" s1
