#use "helpers.ml"

let line_to_pair line =
    (* 99-99,18-99 *)
    Scanf.sscanf line "%d-%d,%d-%d" (fun a1 a2 b1 b2 -> [[a1;a2];[b1;b2]]);;

let fully_contains pair =
    match pair with
    | [[l1;r1];[l2;r2]] -> 
       (l1 - l2) * (r1 - r2) <= 0;;

let overlaps pair =
    match pair with
    | [[l1;r1];[l2;r2]] -> 
       (l1 - r2) * (r1 - l2) <= 0;;

let lines = List.map line_to_pair @@ file_to_lines "input"

(* In how many assignment pairs does one range fully contain the other? *)
let ans1 = List.length @@ List.filter fully_contains lines;;
Printf.printf "Part 1 answer: %d\n" ans1;; (*424*)

(* In how many assignment pairs do the ranges overlap? *)
let ans2 = List.length @@ List.filter overlaps lines;;
Printf.printf "Part 2 answer: %d\n" ans2;; (*804*)
