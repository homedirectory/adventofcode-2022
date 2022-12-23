module Lzl = Aoc.Lzl
open Aoc.Helpers
open Printf

let filename = input_for_day 16
let filename_test = test_input_for_day 16

type valve = {
    name: string;
    rate: int;
    (* mutable opened: bool; *)
    mutable valves: valve list
};;

let mk_valve name rate valves =
    { name=name; rate=rate; valves=valves }
;;

let valve_eq vlv1 vlv2 =
    vlv1.name = vlv2.name
;;

(*
let open_valve vlv =
    vlv.opened <- true
;;
*)

(* DEBUG *)
let find_valve name valves =
    match List.find_opt (fun vlv -> vlv.name = name) valves with
    | None -> failwith "find_valve: Not_found"
    | Some vlv -> vlv
;;

let list_without excluded lst =
    List.filter (fun x -> not (List.mem x excluded)) lst
;;

(* BFS *)
let distance (src: valve) dst =
    let rec iter n valves =
        if List.mem dst valves
        then n
        else
            let next = List.flatten @@
                List.map (fun vlv -> list_without valves vlv.valves) valves in
            iter (n+1) next
    in
    iter 0 [src];;
;;

(*
let valve_score rem_t (src: valve) dst =
    if dst.opened then 0.
    else
        let d = (distance src dst) + 1 in
        if d >= rem_t then 0.
        else (float_of_int dst.rate) /. (float_of_int d)
;;

type action =
    | Move of valve * int
    | Open
    | Idle
;;
*)

(*
let closed_valves valves =
    List.filter (fun vlv -> not vlv.opened) valves 
;;
*)

(*
let maximize f = function
    | [] -> failwith "maximize: empty list"
    | lst -> match list_sortf_desc f lst with
        | h :: t -> h
        | [] -> assert false
;;
*)

let lazy_permutations lst =
    let rec iter = function
        | Lzl.Nil -> Lzl.Nil
        | Lzl.Cons(hd, Nil) as lzl -> Lzl.return (lazy lzl)
        | lzl -> 
            Lzl.map_rest lzl ~f:(fun hd tl -> printf "hd: %d\n" hd; Lzl.map (iter tl) ~f:(Lzl.cons (lazy hd)))
            |> Lzl.flatten
    in
    match lst with
    | [] -> Lzl.empty()
    | hd :: [] -> Lzl.of_list [[hd]]
    | lst -> Lzl.map ~f:Lzl.to_list (iter (Lzl.of_list lst))
;;

let build_paths start_vlv valves : (valve list Seq.t) =
    let non_zero_valves = List.filter (fun v -> v.rate > 0) valves in
    (* let other_valves = list_without [start_vlv] valves in *)
    let permuts = lazy_permutations non_zero_valves in
    Lzl.map (Lzl.cons (lazy start_vlv)) permuts
;;

(* first node in the path is always the start node *)
let total_of_path time (path: valve list) =
    let rec iter prev_vlv rem_t acc = function
        | [] -> acc
        | hd :: tl ->
            let d = (distance prev_vlv hd) + 1 in
            if d >= rem_t
            then acc
            else 
                let new_rem_t = rem_t - d in
                iter hd new_rem_t (acc + (hd.rate * new_rem_t)) tl
    in
    match path with
    | [] -> 0
    | _ :: [] -> 0
    | hd :: tl -> iter hd time 0 tl
;;

let seq_maximize f init sq =
    Seq.fold_left (fun acc x -> max acc (f x)) init sq
;;

let solve time start_vlv valves =
    (*
    1. Find all possible paths (maybe prune?)
    2. Map each path to total pressure released
    3. Maximize
     *)
    let paths = build_paths start_vlv valves in
    printf "%d paths were built\n%!" (Seq.length paths);
    seq_maximize (fun pth -> 
        print_list (fun vlv -> print_string vlv.name) pth;
        let tot = total_of_path time pth in
        printf "total: %d\n%!" tot;
        tot) 0 paths
    (* let totals = Seq.map (fun p -> total_of_path time p) paths in *)
    (* list_max (List.of_seq totals) *)
;;

(*
let solve time start_vlv valves =
    let act rem_t vlv =
        match (closed_valves valves) with
        | [] -> Idle
        | dests ->
            let best_vlv = maximize (valve_score rem_t vlv) dests in
            if valve_eq best_vlv vlv
            then Open
            else Move(best_vlv, (distance vlv best_vlv))
    in
    (*
    1. calculate total pressure
    2. check time
    3. perform action (move/open valve)
    *)
    let rec step rem_t total per_sec vlv =
        printf "Rem. time: %d; total: %d; per_sec: %d; valve: %s\n%!" rem_t total per_sec vlv.name;
        if rem_t = 0 then total
        else 
            match act rem_t vlv with
            | Idle -> total + (rem_t * per_sec)
            | Move(dst_vlv, dist) -> step (rem_t - dist) (total + (dist * per_sec)) per_sec dst_vlv
            | Open -> 
                open_valve vlv;
                step (rem_t - 1) (total + per_sec) (per_sec + vlv.rate) vlv
    in
    step time 0 0 start_vlv
;;
*)

(* PARSING *)
let parse_names str =
    Str.split (Str.regexp ", ") (String.trim str);;

let matched_group n str =
    try Str.matched_group n str with
    | Not_found -> failwith (sprintf "group %d not found in %s" n str);;

let re = Str.regexp {|^Valve \(..\) has flow rate=\([0-9]+\); tunnels? leads? to valves? \(.*\)$|};;
let parse_line ln =
    let _ = Str.search_forward re ln 0 in
    let name = matched_group 1 ln in
    let rate = int_of_string @@ matched_group 2 ln in
    let tunnels = parse_names @@ matched_group 3 ln in
    (name, rate, tunnels);;

let parse_lines lines =
    let parsed = List.map parse_line lines in
    let (valves,tunnels) = List.split @@ List.map 
        (fun (nm,r,tuns) -> (mk_valve nm r [], tuns)) parsed in
    let find_valves tuns =
        List.filter (fun vlv -> List.mem vlv.name tuns) valves
    in
    List.iter2 (fun vlv tuns -> vlv.valves <- (find_valves tuns)) valves tunnels;
    valves;;

let valves = parse_lines (file_to_lines filename_test);;
(* let dd = find_valve "DD" valves;; *)
(* let hh = find_valve "HH" valves;; *)
let n = solve 30 (List.nth valves 0) valves;;
printf "Part 1: %d\n" n;;
