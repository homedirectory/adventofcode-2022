open Aoc.Helpers
open Printf

module SNAFU = struct
    let char_to_dec = function
        | '=' -> -2
        | '-' -> -1
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | c -> failwith (sprintf "char_to_dec: wtf %C" c)

    let to_int str =
        let n = ((String.length str) - 1) in
        str_explode str
        |> List.mapi (fun i c -> (pow 5 (n - i)) * (char_to_dec c))
        |> List.fold_left (+) 0

    let dec_to_char = function
        | 0 -> '0'
        | 1 -> '1'
        | 2 -> '2'
        | 3 -> '='
        | 4 -> '-'
        | d -> failwith (sprintf "dec_to_char: wtf %d" d)

    let to_base5 d =
        let rec iter n acc =
            if n = 0 then acc
            else iter (n / 5) ((n mod 5) :: acc)
        in
        iter d []

    let balance lst =
        let rec iter carry acc = function
            | [] -> if carry > 0 then carry :: acc else acc
            | hd :: tl ->
                let d = hd + carry in
                if d = 5 then iter 1 (0 :: acc) tl
                else if d >= 3 then iter 1 (d :: acc) tl
                else iter 0 (d :: acc) tl
        in
        iter 0 [] (List.rev lst)

    (* first, convert to base 5, then make it balanced *)
    let of_int d =
        str_of_chars (List.map dec_to_char (balance (to_base5 d)))

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
let _ = assert (s1 = "2-0-0=1-0=2====20=-2")
