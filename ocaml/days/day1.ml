open Aoc.Helpers

let filename = input_for_day 1

let solve lines =
    List.map list_sum (List.map (List.map int_of_string) (list_split lines ""));;
let totals = solve (file_to_lines filename);;
let top1 = list_max totals;;
Printf.printf "Top 1 elf total: %d\n" top1;; 
let top3 = list_sum (take 3 (List.sort (fun x y -> (Int.compare x y) * -1) totals));;
Printf.printf "Top 3 elves total: %d\n" top3;; 
