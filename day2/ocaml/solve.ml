(*open Helpers;;*)
#use "helpers.ml"

exception Unknown_letter;;

(* === SHAPES === *)
type shape = Rock | Paper | Scissors;;
let shape_score shape = 
    match shape with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3;;

let shape_loser shape =
    match shape with
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper;;

let shape_winner shape =
    match shape with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock;;

(* === RULES === *)
type outcome = Win | Draw | Lose;;
let round_outcome them us =
    match (them, us) with
        | (Paper, Rock) | (Rock, Scissors) | (Scissors, Paper) -> Lose
        | (Paper, Paper) | (Rock, Rock) | (Scissors, Scissors) -> Draw
        | (_, _) -> Win;;

(* shapes is a list of 2 shapes [them,us] *)
let round_score shapes =
    let them, us = (List.hd shapes), (List.nth shapes 1) in
        shape_score us + match round_outcome them us with
            | Win -> 6
            | Draw -> 3
            | Lose -> 0;;

let them_letter_to_shape letter =
    match letter with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> raise Unknown_letter;;

(* === PART 1 STRATEGY === *)
let us_letter_to_shape1 letter = 
    match letter with
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | _ -> raise Unknown_letter;;

let letters_to_shapes1 two_letters = 
    [(them_letter_to_shape @@ List.hd two_letters); 
     (us_letter_to_shape1 @@ List.nth two_letters 1)];;


(* === PART 2 STRATEGY === *)
let us_letter_to_shape2 us = 
    match us with
        | "X" -> shape_loser
        | "Y" -> (fun x -> x) 
        | "Z" -> shape_winner
        | _ -> raise Unknown_letter;;

let letters_to_shapes2 two_letters = 
    let them, us = (List.hd two_letters), (List.nth two_letters 1) in
        let them_shape = (them_letter_to_shape them) in
        [them_shape; us_letter_to_shape2 us them_shape];;

(* === READ INPUT === *)
let lines = file_to_lines "input";;
let letters = List.map (String.split_on_char ' ') lines;;

(* letters_to_shapes is a function taking a pair of letters 
   and returning a pair of shapes *) 
let solve letters_to_shapes_f =
    list_sum @@ List.map round_score @@ List.map letters_to_shapes_f letters;;

Printf.printf "Strategy 1 score: %d\n" @@ solve letters_to_shapes1;;
Printf.printf "Strategy 2 score: %d\n" @@ solve letters_to_shapes2;;
