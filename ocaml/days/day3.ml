open Aoc.Helpers

let filename = input_for_day 3

let c0 = Char.chr 0;;
let alphabet_full = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'];;

let priority c = 
    let code = Char.code c in
        if isupper c then code - 38 else code - 96;;

(* index in alphabet: [A,...,Z,a,...,z] *)
let char_to_index c = 
    let code = Char.code c in
        if isupper c then code - 65 else code - 71;;

let alphabet_of_str s =
    let alph = Array.make 52 c0 in
        List.iter (fun c -> alph.(char_to_index c) <- c) @@ str_explode s;
        Array.to_list alph;;

let alphabet_match a1 a2 =
    List.map2 (fun c1 c2 -> if c1 = c2 then c1 else c0) a1 a2;;

let alphabet_first alph = List.find_opt (fun c -> c != c0) alph;;

(* find the first character that appears in all strings *)
let find_common_char_opt strings =
    alphabet_first @@ 
    List.fold_left alphabet_match alphabet_full @@ List.map alphabet_of_str strings;;

let find_common_char strings = Option.get @@ find_common_char_opt strings;;

let common_item sack =
    let half = String.length sack / 2 in 
        let left, right = String.sub sack 0 half, String.sub sack half half in
            find_common_char [left;right];;

(* === SOLVE === *)
let rucksacks = file_to_lines filename;;
Printf.printf "Part 1 answer: %d\n" @@
    list_sum @@ List.map priority @@ List.map common_item rucksacks;;

Printf.printf "Part 2 answer: %d\n" @@
list_sum @@ List.map priority @@ List.map find_common_char @@ list_windowed 3 rucksacks;;

(* a cool way to make chunks of 3 but may result into Match_failure for unexpected input *)
let rec chunks_of_3 lst = 
    match lst with
        | [] -> []
        | a :: b :: c :: rest -> [[a;b;c]] @ (chunks_of_3 rest);;
