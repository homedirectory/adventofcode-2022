(*#use "helpers.ml";;*)
open Helpers;;

type step =
    | Left of int
    | Right of int
    | Up of int
    | Down of int;;

let get_step_n stp =
    match stp with
    | Left n -> n
    | Right n -> n
    | Up n -> n
    | Down n -> n;;

let step_to_str stp =
    match stp with
    | Left n -> Printf.sprintf "Left %d" n
    | Right n -> Printf.sprintf "Right %d" n
    | Up n -> Printf.sprintf "Up %d" n
    | Down n -> Printf.sprintf "Down %d" n;;

let print_step stp =
    print_endline (step_to_str stp);;

(* copy step but with a different number *)
let step_n n stp = 
    match stp with
    | Left _ -> Left n
    | Right _ -> Right n
    | Up _ -> Up n
    | Down _ -> Down n;;

let get_x (x,y) = x;;
let get_y (x,y) = y;;

let move stp (x,y) =
    (*Printf.printf "move %s (%d,%d)\n%!" (step_to_str stp) x y;*)
    match stp with
    | Left n -> (x-n, y)
    | Right n -> (x+n, y)
    | Up n -> (x, y+n)
    | Down n -> (x, y-n);;

let same_row_or_col (x1, y1) (x2, y2) =
    x1 = x2 || y1 = y2;;

let i2f = float_of_int;;
let pow2 fl = Float.pow fl 2.;;
let distance (x1,y1) (x2,y2) =
    (*Printf.printf "distance (%d,%d) (%d,%d)\n%!" x1 y1 x2 y2;*)
    let x1f, y1f, x2f, y2f = i2f x1, i2f y1, i2f x2, i2f y2 in  
    int_of_float @@ Float.floor @@ Float.sqrt((pow2 (x1f -. x2f)) +. (pow2 (y1f -. y2f)));;

(* return new position of t *)
let move_diag_to (tx,ty) (hx,hy) =
    (*print_endline "moving diagonally";*)
    if tx = hx || ty = hy 
    (* edge case *)
    then (tx, ty)
    else
        (* t moves to h by 1 *)
        let up = hy > ty in
        let right = hx > tx in
        match up, right with
        | true, true -> (tx+1, ty+1)
        | true, false -> (tx-1, ty+1)
        | false, true -> (tx+1, ty-1)
        | false, false -> (tx-1, ty-1);;

(* t and h are either in the same row or in the same column 
    and the distance between them is greater than 1 *)
let move_to (tx,ty) (hx,hy) =
    if tx = hx && ty = hy
    then (tx, ty)
    else (
        if hx = tx
        (* same column *)
        then (
            (*print_endline "moving vertically"; *)
              if ty < hy then (tx, hy-1) else (tx, hy+1))
        (* same row *)
    else (
        (*print_endline "moving horizontally"; *)
            if tx < hx then (hx-1, ty) else (hx+1, ty))
    );;

(* moves tail depending on the position of head *)
(* stp must be a step of 1 *)
let one_step stp tpos hpos =
    let dist = distance tpos hpos in
    (*Printf.printf "distance after H moved: %d\n%!" dist;*)
    if dist <= 1 then tpos
    else (if same_row_or_col tpos hpos
          then (
              (*print_endline "same row or col";*)
              (move_to tpos hpos))
          else (move_diag_to tpos hpos));;

let divide_step stp =
    let n = get_step_n stp in
    List.init n (fun _ -> step_n 1 stp);;

(* knots is a list of knot positions starting from head *)
(* stp should be a step of 1 *)
let apply_step stp knots =
    (* first move the head, independetly of other knots *)
    match knots with
    | [] -> raise (Error "apply_step: knots are empty")
    | h :: t -> let head = move stp h in
        (* other knots move depending on the position of the previosuly moved knot *)
        let result = List.fold_left (fun moved curr -> let prev = List.hd moved in
            (one_step stp curr prev) :: moved) 
            [head] t in
        (* reverse in the end to restore knot order *)
        List.rev result;;

let simulate n step_list =
    (* prev is a list of knots starting from head *)
    let rec iter prev steps =
        match steps with
        | [] -> [prev]
        | h :: t -> let this = apply_step h prev in
            prev :: (iter this t)
        in
    iter (List.init n (fun _ -> (0, 0))) (List.flatten @@ List.map divide_step step_list);;

(*let lines = file_to_lines "input";;*)
let lines = file_to_lines Sys.argv.(1);;
let steps = List.map 
    (fun ln -> match (String.split_on_char ' ' ln) with
        | "L"::n::[] -> Left (int_of_string n)
        | "R"::n::[] -> Right (int_of_string n)
        | "D"::n::[] -> Down (int_of_string n)
        | "U"::n::[] -> Up (int_of_string n)
        | _ -> raise (Error "bad input"))
    lines;;

let solve n =
    (* a list of lists [posH; pos1; pos2; ...] *)
    print_endline "performing steps...";
    let coords = simulate n steps in
    print_endline "DONE performing steps";
    let x_min = list_min (List.map (fun positions -> list_min (List.map get_x positions)) coords) in
    let y_min = list_min (List.map (fun positions -> list_min (List.map get_y positions)) coords) in

    let x_add = if x_min < 0 then -1 * x_min else 0 in
    let y_add = if y_min < 0 then -1 * y_min else 0 in

    let coords = List.map (fun positions -> 
        List.map (fun (x,y) -> (x + x_add, y + y_add)) positions) coords in

    let x_max = list_max (List.map (fun positions -> list_max (List.map get_x positions)) coords) in
    let y_max = list_max (List.map (fun positions -> list_max (List.map get_y positions)) coords) in

    (* lst is a list of positions tail visits *)
    let count_positions lst = 
        let matr = Array.make_matrix (y_max + 1) (x_max + 1) 0 in
        List.iter (function (x, y) -> Printf.printf "%d, %d\n%!" x y; matr.(y_max - y).(x) <- 1) lst;
        Array.fold_left (+) 0 (Array.map (Array.fold_left (+) 0) matr) in

    let tail_positions = List.map list_last coords in
    count_positions tail_positions;;

let part1 = (solve 2);;
let part2 = (solve 10);;
(*let crazy = (solve 100);;*)
Printf.printf "Part 1: %d\n%!" part1;;
Printf.printf "Part 2: %d\n%!" part2;;
(*Printf.printf "Crazy: %d\n%!" crazy;;*)
