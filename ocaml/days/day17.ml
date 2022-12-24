open Printf
open Aoc.Helpers

let filename = input_for_day 17
let filename_test = test_input_for_day 17

type shape =
    | HLine
    | Plus
    | FlippedL
    | VLine
    | Square

let right n (x, y) =
    List.init n (fun i -> (x+i, y))

let up n (x, y) =
    List.init n (fun i -> (x, y+i))

let square n (x, y) =
    List.flatten @@ List.map (right n) (up n (x, y))

let shape_coords_map (x, y) = function
    | HLine -> right 4 (x, y)
    | Plus -> (x+1, y) :: (right 3 (x, y+1)) @ [(x+1, y+2)]
    | FlippedL -> (right 3 (x, y)) @ (up 2 (x+2, y+1))
    | VLine -> up 4 (x, y)
    | Square -> square 2 (x, y)

let stream_shapes () =
    Seq.cycle (List.to_seq [HLine; Plus; FlippedL; VLine; Square])

type direction =
    | Right
    | Left
    | Down

let dir_map_pos (x, y) = function
    | Right -> (x+1, y)
    | Left -> (x-1, y)
    | Down -> (x, y-1)

type pos_t = (int * int)
type rock = { shp: shape; pos: pos_t; coords: pos_t list Lazy.t }

let mk_rock shp pos =
    { shp = shp; pos = pos; coords = lazy (shape_coords_map pos shp) }

let rock_x { shp=_; pos=(x,_) ; coords=_ } =
    x

let rock_y { shp=_; pos=(_,y); coords=_ } =
    y

let rock_coords { shp=_; pos=_; coords = lazy coords } =
    coords

let rock_top_y rk =
    list_max (List.map (fun (_, y) -> y) (rock_coords rk))

let rock_bot_y rk =
    list_min (List.map (fun (_, y) -> y) (rock_coords rk))

let rock_move dir rk =
    mk_rock rk.shp (dir_map_pos rk.pos dir )

let rock_to_str { shp=_; pos=(x,y); coords=_ } =
    sprintf "pos=(%d,%d)" x y

type cell_t =
    | Rock
    | Empty

let cell_is_empty = function
    | Rock -> false
    | Empty -> true

let cell_to_str = function
    | Rock -> "#"
    | Empty -> "."

let print_cell cell =
    print_string (cell_to_str cell)

type 'a matrix = 'a array array

type wall = { width: int; height: int; real_height: int; rows: cell_t matrix }

let mk_wall w h rh rows =
    { width=w; height=h; real_height=rh; rows=rows }

let mk_empty_wall w =
    mk_wall w 0 0 (Array.make_matrix 0 0 Empty)

let wall_add_row wl row =
    { wl with rows = matrix_append_row row wl.rows }

let wall_add_height dh wl =
    { wl with height = wl.height + dh; real_height = wl.real_height + dh }

let wall_set_rows rows wl =
    { wl with rows = rows}

let wall_expand_for_rock rk wl =
    let wall_add_empty_row wl_in =
        wall_add_row wl_in (Array.make (wl_in.width) Empty)
    in
    let y_max = rock_top_y rk in
    let dy = max 0 (y_max - (wl.height - 1)) in
    fun_fold wall_add_empty_row dy wl

let wall_put_rock rk wl =
    let coords = rock_coords rk in
    let rows_out = List.fold_left (fun rows_acc pos -> matrix_put pos Rock rows_acc) wl.rows coords in
    wall_set_rows rows_out wl


let wall_add_rock rk wl =
    let update_height rk_in wl_in =
        let top_y = rock_top_y rk_in in
        let dh = max 0 (top_y - (wl_in.height - 1)) in
        wall_add_height dh wl_in
    in
    let wl_exp = wall_expand_for_rock rk wl in
    let wl_out = wall_put_rock rk wl_exp in
    update_height rk wl_out

let wall_cell_at (x, y) wl =
    if x < 0 || x >= wl.width
    then failwith (sprintf "wall_cell_at: X=%d is out of bounds" x)
    else if y < 0 then failwith (sprintf "wall_cell_at: Y=%d is < 0" y)
    else if y >= wl.height then Empty
    else matrix_get (x, y) wl.rows

let wall_cells_at coords wl =
    List.map (fun pos -> wall_cell_at pos wl) coords

let wall_drop_rows n wl =
    { wl with rows = arr_drop n wl.rows; height = wl.height - n }

(* if some row is fully packed with rocks the wall can be shrinked
   so that row becomes the new floor,
   y - denotes the start of row range to check (ends at the top) *)
let wall_shrink y wl =
    let is_full_row row =
        not (Array.mem Empty row)
    in
    let y_max = wl.height - 1 in
    match matrix_range_find_row_idx_opt is_full_row y y_max wl.rows with
    | None -> wl
    | Some i -> 
        (*printf "shrinking wall by %d\n" i;*)
        wall_drop_rows i wl

let print_wall wl =
    print_matrix print_cell "" wl.rows;
    print_newline()


type rock_info =
    | InWall of rock
    | InFloor of rock
    | InRock of rock
    | Valid of rock

(* rock is valid if the following are false:
    - part of the rock is inside a wall
    - part of the rock is inside another rock
    - part of the rock is inside the floor
    *)
let rock_wall_info rk wl =
    let is_in_wall (x, _) =
        x < 0 || x >= wl.width 
    in
    let is_in_floor (_, y) =
        y < 0
    in
    let is_in_another_rock pos =
        not (cell_is_empty (wall_cell_at pos wl))
    in
    let coords = rock_coords rk in
    if list_memf is_in_wall coords then InWall rk
    else if list_memf is_in_floor coords then InFloor rk
    else if list_memf is_in_another_rock coords then InRock rk
    else Valid rk

let move rk dir wl =
    let moved_rk = rock_move dir rk in
    rock_wall_info moved_rk wl

let fall rk wl (dirs: direction Seq.t) : (rock * direction Seq.t) =
    let foldf rk_in dir =
        (*printf "falling\n%!";*)
        match move rk_in dir wl with
        | Valid rk_out -> Some rk_out
        | InWall _ -> Some rk_in
        | InFloor _ -> None
        | InRock _ ->
            match dir with
            | Down -> None
            | _ -> Some rk_in
    in
    let (stopped_rk, rest_dirs) = seq_foldl_take foldf rk dirs in
    (stopped_rk, rest_dirs)

let wall_width = 7

let spawn_rock shp wl =
    mk_rock shp (2, wl.height + 3)

let simulate max_rocks (shapes: shape Seq.t) (dirs: direction Seq.t) =
    let print_step = max 1 (max_rocks / 1000) in
    let foldf (wl_acc, dirs_acc) i shp =
        (* print_wall wl_acc; Unix.sleepf 0.2; *)
        if i = max_rocks
        then None
        else
            begin
                if i mod print_step = 0 then printf "%d rocks have stopped\n%!" i else ();
                let rk = spawn_rock shp wl_acc in
                (*printf "Spawned rock: %s\n%!" (rock_to_str rk);*)
                let (stopped_rk, rest_dirs) = fall rk wl_acc dirs_acc in
                let wl_out = wall_add_rock stopped_rk wl_acc 
                    |> wall_shrink (rock_bot_y stopped_rk) in
                (*printf "Landed rock: %s\n%!" (rock_to_str stopped_rk);*)
                (* printf "Wall height: %d\n" wl_acc.height;*)
                Some(wl_out, rest_dirs)
            end
    in
    let wl = mk_empty_wall wall_width in
    let ((wl_out, _), _) = seq_foldli_take foldf (wl, dirs) shapes in
    wl_out

let parse_jets_to_dirs input =
    let dir_of_jet_char = function
        | '<' -> Left
        | '>' -> Right
        | c -> failwith (sprintf "dir_of_jet_char: unknown char '%c'" c)
    in
    str_explode input |> List.map dir_of_jet_char

let stream_dirs jet_dirs_lst =
    let jet_dirs_sq = Seq.cycle (List.to_seq jet_dirs_lst) in
    Seq.interleave jet_dirs_sq (Seq.repeat Down)

let part1 input =
    let jet_dirs = parse_jets_to_dirs input in
    let wl = simulate 2022 (stream_shapes()) (stream_dirs jet_dirs) in
    printf "Wall real height: %d\n" wl.real_height;
    wl

let part2 input = 
    let jet_dirs = parse_jets_to_dirs input in
    let wl = simulate 1000000000000 (stream_shapes()) (stream_dirs jet_dirs) in
    printf "Wall real height: %d\n" wl.real_height;
    wl

let test_input = file_to_text filename_test |> String.trim
let input = file_to_text filename |> String.trim
let inp = input
(*let _ = part1 inp*)
let wl2 = part2 inp
