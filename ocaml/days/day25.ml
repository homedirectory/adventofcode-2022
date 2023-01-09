open Aoc.Helpers
open Printf

let log5 a =
    Float.to_int (logf 5. (Float.of_int a))

let div_rem a b =
    let q = a / b in
    let r = a - (q * b) in
    (q, r)

module SNAFU = struct
    let char_to_int = function
        | '=' -> -2
        | '-' -> -1
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | c -> failwith (sprintf "char_to_int: wtf %C" c)

    let int_to_char = function
        | -2 -> '='
        | -1 -> '-'
        | 0 -> '0'
        | 1 -> '1'
        | 2 -> '2'
        | d -> failwith (sprintf "int_to_char: wtf %d" d)

    let to_int str =
        let n = ((String.length str) - 1) in
        str_explode str
        |> List.mapi (fun i c -> (pow 5 (n - i)) * (char_to_int c))
        |> List.fold_left (+) 0

    let length_of_int d =
        (log5 (d * 2)) + 1

    let rev_char = function
        | '=' -> '2'
        | '-' -> '1'
        | '0' -> '0'
        | '1' -> '-'
        | '2' -> '='
        | c -> failwith (sprintf "char_rev: wtf %C" c)

    let of_int d =
        let rec max_val e =
            if e = 0 then 2
            else (2 * (pow 5 e)) + (max_val (e - 1))
        in
        let map_neg neg c =
            if neg then rev_char c else c
        in
        let mid a b =
            a + ((b - a) / 2) + 1
        in
        let rec iter n acc x neg =
            printf "iter n=%d acc='%s' x=%d neg=%B\n%!" n (str_of_chars (List.rev acc)) x neg;
            if n < 0 then begin assert (x <= 0); List.rev acc end 
            else
                if x = 0 then (List.rev acc) @ (list_make (n + 1) '0')
                else if n > 0 && x <= (max_val (n - 1)) then iter (n - 1) ('0' :: acc) x neg
                else
                    let e = pow 5 n in
                    let qf = (Float.of_int x) /. (Float.of_int e) in
                    let r = x - ((Float.to_int qf) * e) in
                    if qf <= 1.0 then iter (n - 1) ((map_neg neg '1') :: acc) (e - x) (not neg)
                    else if qf <= 1.5 then iter (n - 1) ((map_neg neg '1') :: acc) r neg
                    else if qf < 2. then iter (n - 1) ((map_neg neg '2') :: acc) ((e * 2) - x) (not neg)
                    else iter (n - 1) ((map_neg neg '2') :: acc) (x - (e * 2)) neg
        in
        str_of_chars (iter ((length_of_int d) - 1) [] d (d < 0))
end

let part1 input =
    let lines = Str.split (Str.regexp_string "\n") input in
    let ints = List.map SNAFU.to_int lines in
    let s = list_sum ints in
    SNAFU.of_int s

let test_input = file_to_text (test_input_for_day 25)
let test_s1 = part1 test_input
let _ = printf "Part 1 test: %s\n" test_s1
let _ = assert (test_s1 = "2=-1=0")

let input = file_to_text (input_for_day 25)
let s1 = part1 input
let _ = printf "Part 1: %s\n" s1
(*let _ = assert (s1 = "2=-1=0")*)
