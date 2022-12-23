open Aoc.Helpers

let filename = input_for_day 6

module Char_set = Set.Make (struct 
    type t = char 
    let compare = compare 
end);;

let uniq chars =
    (List.length chars) = (Char_set.cardinal (Char_set.of_list chars));;

(*let chars = str_explode @@ String.concat "" (file_to_lines Sys.argv.(1));;*)
let text = String.concat "" (file_to_lines Sys.argv.(1));;
Printf.printf "Input length: %d\n%!" (String.length text);;

List.iteri (fun part n ->
    let opt = string_findi (fun i ->
        let window = String.sub text i n in
        uniq (str_explode window)
    ) text in
    match opt with
    | Some pos -> Printf.printf "Part %d: %d\n%!" (part+1) (pos+n)
    | None -> Printf.printf "Part %d: no answer\n%!" (part+1)
) [4;14];;
