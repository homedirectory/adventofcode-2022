open Printf;;
#use "helpers.ml";;

type 'a matrix = 'a array list list

type pos = int * int

type line =
    | Line of pos array

type path =
    | Path of line array

let pos_of_str s =
    match String.split_on_char ',' s with
    | [x;y] -> (int_of_string x, int_of_string y)
    | _ -> assert false 

(* This will only work for straight lines *)
let line_of_pos (p1,p2) =
    let make = function
        | (xmin, ymin), (xmax, ymax) ->
            let [xrange;yrange] = lists_allign_len (fun lst d -> list_make d (List.hd lst)) 
                [(range_incl xmin xmax);(range_incl ymin ymax)] in
            Line(Array.of_list @@ List.map2 (fun x y -> (x,y)) xrange yrange)
    in
    match p1, p2 with
    | (x1,y1), (x2,y2) -> make (((min x1 x2),(min y1 y2)), ((max x1 x2),(max y1 y2)))

let path_of_positions arr =
    let pos_pairs = list_windowmap list_to_pair 2 (Array.to_list arr) in
    let lines = List.map line_of_pos pos_pairs in
    Path(Array.of_list lines)

let rock_chr, air_chr, src_chr, sand_chr = '#', '.', '+', 'o'

let grid_set (x,y) v grid =
    grid.(y).(x) <- v

let grid_get (x,y) grid =
    try Some(grid.(y).(x)) with Invalid_argument _ -> None

let print_grid grid =
    print_matrix print_char "" grid; print_newline()

let rec list_step f =
    match f() with
    | None -> []
    | Some s -> s :: list_step f

type square =
    | Air of pos
    | Void

let simulate grid src_pos =
    let next_pos (x,y) =
        List.find_map (fun pos -> 
            match grid_get pos grid with
            | None -> Some Void
            | Some '.' -> Some(Air(pos))
            | _ -> None)
        [(x,y+1);(x-1,y+1);(x+1,y+1)]
    in
    let rec sim_unit () =
        (* Unix.sleepf(0.025); *)
        (* print_grid grid; *)
        let rec step (x,y) =
            (* printf "(%d,%d)\n" x y; *)
            match next_pos (x,y) with
            | None -> Some (x,y)
            | Some(Air pos) -> step pos
            | Some Void -> None
        in
        match step src_pos with
        | None -> None
        | Some pos -> grid_set pos sand_chr grid; Some pos
    in
    list_step sim_unit

let find_min_pos posits =
    Array.fold_left (fun (acc_x,acc_y) (x,y) -> (min acc_x x, min acc_y y)) 
        (Int.max_int, Int.max_int) posits

let find_max_pos posits =
    Array.fold_left (fun (acc_x,acc_y) (x,y) -> (max acc_x x, max acc_y y)) 
        (Int.min_int, Int.min_int) posits

(* let filename = "input" *)
let filename = "input.test"
let input_lines = Array.of_list @@ file_to_lines filename

(* adjust all positions to start at either x=0 or y=0 *)
let re = Str.regexp_string " -> "
let lines = Array.map (fun ln -> Array.of_list @@ Str.split re ln) input_lines
(* matrix of positions *)
let positions = matrix_map pos_of_str lines
let all_positions = matrix_flatten positions
(* we know that sand is pouring from (500,0) so min_y = 0 *)
let (min_x,min_y) = 
    match find_min_pos all_positions with
    | x, _ -> (x, 0)

let (max_x, max_y) = find_max_pos all_positions

let adj_pos (x,y) =
    (x - min_x, y - min_y)

let adj_positions = matrix_map adj_pos positions
let paths = Array.map path_of_positions adj_positions
let src_pos = adj_pos (500, 0)

let grid = Array.make_matrix (max_y - min_y + 1) (max_x - min_x + 1) air_chr
let _ = grid_set src_pos src_chr grid

let _ = Array.iter (function Path lines -> 
    Array.iter (function Line posits -> 
        Array.iter (fun pos -> grid_set pos rock_chr grid) posits) 
    lines)
paths

let sand_positions = simulate grid src_pos
let _ = printf "Part 1: %d\n" (List.length sand_positions)
