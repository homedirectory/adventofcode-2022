open Aoc.Helpers
open Printf

module type BFS_t = sig
    val search : ('a -> 'a) -> ('a -> 'b -> 'b list) -> ('b -> bool) -> 
        ('b list -> 'b list) -> 'a -> 'b -> (int * 'a) option
end

module BFS : BFS_t = struct
    (* statef - state step function
       solf - solution one-to-many step function
       solp - predicate applied to a solution, tells when to stop
       sol_prunef - pruning function for solutions
       state - initial state 
       sol - initial solution *)
    let search statef solf solp sol_prunef state sol =
        let rec iter i state_acc = function
            | [] -> None
            | sols -> begin
                printf "BFS: num_sols=%d depth=%d\n%!" (List.length sols) i;
                match List.find_opt solp sols with
                | Some sm_sol -> Some (i, state_acc)
                | None ->
                    let next_state = (statef state_acc) in
                    let next_sols = List.map (solf next_state) sols
                        |> List.flatten |> sol_prunef in
                    iter (i + 1) next_state next_sols
            end
        in
        iter 0 state [sol]
end

type pos_t = int * int

let pos_to_str (x, y) =
    sprintf "(%d, %d)" x y

let print_pos pos =
    print_string (pos_to_str pos)

let adjacent_pos (x, y) =
    [
        (x + 1, y);
        (x - 1, y);
        (x, y + 1);
        (x, y - 1)
    ]

module Bliz = struct
    type t = Up | Down | Right | Left

    let next_pos (x, y) = function
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Right -> (x + 1, y)
        | Left -> (x - 1, y)

    let to_str = function
        | Up -> "^"
        | Down -> "v"
        | Right -> ">"
        | Left -> "<"

    let print bliz =
        print_string (to_str bliz)
end

type tile_t = 
    | Wall
    | Blizzards of Bliz.t list

let tile_to_str = function
    | Wall -> "#"
    | Blizzards [] -> "."
    | Blizzards [b] -> Bliz.to_str b
    | Blizzards lst -> string_of_int (List.length lst)

let print_tile tile =
    print_string (tile_to_str tile)

let tile_is_ground = function
    | Wall -> false
    | Blizzards [] -> true
    | Blizzards _ -> false

let tile_is_wall = function
    | Wall -> true
    | _ -> false

let tile_of_char = function
    | '#' -> Wall
    | '.' -> Blizzards []
    | '^' -> Blizzards [Bliz.Up]
    | 'v' -> Blizzards [Bliz.Down]
    | '>' -> Blizzards [Bliz.Right]
    | '<' -> Blizzards [Bliz.Left]
    | c -> failwith (sprintf "tile_of_char: wtf is %C" c)

type grid_t = { tiles: tile_t array array }

let print_grid grid pos =
    let g = matrix_map_copy tile_to_str grid.tiles in
    let _ = matrix_put pos "E" g in
    print_matrix print_string "" g

let grid_of_tiles tiles =
    { tiles }

let grid_get_opt grid pos =
    matrix_get_opt pos grid.tiles

let grid_put grid pos tile =
    ignore (matrix_put pos tile grid.tiles)

let grid_dims grid =
    matrix_dims grid.tiles

let grid_find_src grid =
    let row = List.mapi (fun i tile -> (i, tile)) (Array.to_list grid.tiles.(0))
        |> List.filter (fun (_, tile) -> tile_is_ground tile)
        |> List.map fst
    in
    assert (1 = (List.length row));
    let x = List.hd row in
    (x, 0)

let grid_find_dst grid =
    let (h, _) = grid_dims grid in
    let row = List.mapi (fun i tile -> (i, tile)) (Array.to_list grid.tiles.(h - 1))
        |> List.filter (fun (_, tile) -> tile_is_ground tile)
        |> List.map fst
    in
    assert (1 = (List.length row));
    let x = List.hd row in
    (x, h - 1)

let grid_put_bliz grid pos bliz =
    let lst = match grid_get_opt grid pos with
        | Some (Blizzards lst) -> lst
        | None -> failwith (sprintf "grid_put_bliz: %s out of bounds" (pos_to_str pos))
        | Some Wall -> failwith (sprintf "grid_put_bliz: %s contains Wall" (pos_to_str pos))
    in
    grid_put grid pos (Blizzards (bliz :: lst))

let grid_wrap_pos grid pos =
    let (h, w) = grid_dims grid in
    let pos_out = 
    match grid_get_opt grid pos with
    | Some (Blizzards _) -> pos
    | None | Some Wall -> 
        let (x, y) = pos in
        if x = 0 then (w - 2, y)
        else if x = w - 1 then (1, y)
        else
            let col = matrix_get_col x grid.tiles in
            let y_out =
                if y <= 1 then arr_findr_idx (Fun.negate tile_is_wall) col
                else if y >= (h - 1) then arr_findl_idx (Fun.negate tile_is_wall) col
                else failwith (sprintf "wtf %s" (pos_to_str pos))
            in
            (x, y_out)
    in
    (*if pos <> pos_out then printf "wrap %s to %s\n%!" (pos_to_str pos) (pos_to_str pos_out) else ();*)
    pos_out

let move_bliz grid pos bliz =
    let next_pos = grid_wrap_pos grid (Bliz.next_pos pos bliz) in
    grid_put_bliz grid next_pos bliz 

let move_blizzards grid =
    let g = grid_of_tiles (matrix_map_copy (function 
        Blizzards _ -> Blizzards [] | Wall -> Wall) 
    grid.tiles) in
    matrix_iteri (fun pos tile ->
        match tile with
        | Blizzards lst -> List.iter (move_bliz g pos) lst
        | Wall -> ())
    grid.tiles;
    g

let find_positions pos grid =
    adjacent_pos pos 
    |> List.filter_map (fun p -> Option.map (fun tile -> (p, tile)) (grid_get_opt grid p))
    |> List.filter (fun (_, tile) -> tile_is_ground tile)
    |> List.map fst

let can_wait pos grid =
    match grid_get_opt grid pos with
    | None -> failwith (sprintf "can_wait: pos %s out of bonds" (pos_to_str pos))
    | Some Wall -> failwith (sprintf "can_wait: pos %s is a Wall" (pos_to_str pos))
    | Some Blizzards [] -> true
    | Some Blizzards _ -> false

let next_posits grid pos =
    (*print_grid grid pos;*)
    let posits = find_positions pos grid in
    if can_wait pos grid then pos :: posits else posits

let parse_input inp =
    let mtrx = Str.split (Str.regexp_string "\n") inp
    |> List.map str_explode
    |> List.map (List.map tile_of_char)
    |> matrix_of_2dlist
    in
    grid_of_tiles mtrx

let part1 inp =
    let grid = parse_input inp in
    let src = grid_find_src grid in
    let dst = grid_find_dst grid in
    printf "source: %s\n" (pos_to_str src);
    printf "destination: %s\n" (pos_to_str dst);

    let pred pos =
        (*print_grid grid pos;*)
        pos = dst
    in

    let prunef posits = 
        List.sort_uniq compare posits
    in
 
    match BFS.search move_blizzards next_posits pred prunef grid src with
    | None -> failwith "Part 1: exit was not found"
    | Some (n, _) -> n

let test_input = file_to_text (test_input_for_day 24)
let test_s1 = part1 test_input
let _ = printf "Part 1 test: %d\n" test_s1
let _ = assert (test_s1 = 18)

let input = file_to_text (input_for_day 24)
let s1 = part1 input
let _ = printf "Part 1: %d\n" s1
let _ = assert (s1 = 247)

let part2 inp =
    let grid = parse_input inp in
    let src = grid_find_src grid in
    let dst = grid_find_dst grid in
    printf "source: %s\n" (pos_to_str src);
    printf "destination: %s\n" (pos_to_str dst);

    let is_dst pos =
        pos = dst
    in

    let is_src pos =
        pos = src
    in

    let prunef posits = 
        List.sort_uniq compare posits
    in
 
    let (n1, grid1) = 
        match BFS.search move_blizzards next_posits is_dst prunef grid src with
        | None -> failwith "Part 2: exit was not found"
        | Some sm -> sm
    in
    printf "Trip 1 took %d minutes\n" n1;
    let (n2, grid2) = 
        match BFS.search move_blizzards next_posits is_src prunef grid1 dst with
        | None -> failwith "Part 2: exit was not found"
        | Some sm -> sm
    in
    printf "Trip 2 took %d minutes\n" n2;
    let (n3, _) = 
        match BFS.search move_blizzards next_posits is_dst prunef grid2 src with
        | None -> failwith "Part 2: exit was not found"
        | Some sm -> sm
    in
    printf "Trip 3 took %d minutes\n" n3;
    n1 + n2 + n3

let test_input = file_to_text (test_input_for_day 24)
let test_s2 = part2 test_input
let _ = printf "Part 2 test: %d\n" test_s2
let _ = assert (test_s2 = 54)

let input = file_to_text (input_for_day 24)
let s2 = part2 input
let _ = printf "Part 2: %d\n" s2
let _ = assert (s1 = 728)
