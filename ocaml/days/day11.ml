open Printf
open Aoc.Helpers

let filename = input_for_day 11

type item = int;;

type monkey = { 
    mutable items: item list;
    op: item -> item; 
    (* returns the monkey number to throw to *) 
    decis: item -> int
};;

let print_monkey mnk =
    printf "[";
    List.iter (printf "%d; ") mnk.items; 
    printf "]";
    print_newline();;

(* iftrue and iffalse are indexes of monkeys to throw item to *)
let mk_decis div_by iftrue iffalse =
    fun itm -> if itm mod div_by = 0 then iftrue else iffalse;;

let map_items f mnk =
    mnk.items <- List.map f mnk.items;;

(* lvl_f is a function that computes the worry level *)
let mnk_inspect lvl_f mnk =
    map_items (fun itm -> lvl_f (mnk.op itm)) mnk;
    List.map mnk.decis mnk.items;;

let pop_item mnk =
    match mnk.items with
    | [] -> failwith "pop_item: no items"
    | h :: t -> mnk.items <- t; h;;

let catch_item mnk itm =
    mnk.items <- mnk.items @ [itm];;

let throw_item from_mnk to_mnk =
    catch_item to_mnk (pop_item from_mnk);;

(* inds is a list of indexes into monkeys list *)
let throw_items mnk inds monkeys =
    List.iter (fun i -> throw_item mnk (List.nth monkeys i)) inds;;

(* lvl_f - computes worry level
   n - number of rounds
   monkeys - initial monkeys *)
let solve lvl_f n monkeys =
    (* n - round number (used for printing messages) *)
    (* returns a list of item lists each monkey had when its turn came *)
    let round n monkeys = 
        let items = List.map (fun mnk -> 
            (* list of monkey indexes to throw to *)
            let inspected = mnk_inspect lvl_f mnk in
            (* remember current items before throwing *)
            let itms = mnk.items in
            throw_items mnk inspected monkeys;
            itms)
        monkeys in
        (* DEBUG *)
        let nn = inc n in
        if nn = 1 || nn = 20 || nn mod 1000 = 0 then (
        printf "------ After round %d ------\n%!" nn;
        List.iter print_monkey monkeys);
        (* END DEBUG *)
        items
    in

    let round_items = List.init n (fun i -> round i monkeys) in
    let round_totals = list_add @@ List.map (List.map List.length) round_items in
    (* DEBUG *)
    List.iteri (printf "Monkey %d inspected %d items\n%!") round_totals;
    (* END DEBUG *)
    (* compute monkey business *)
    match take 2 @@ list_sort_desc round_totals with
    | t1 :: t2 :: [] -> t1 * t2
    | _ -> failwith "there must be exactly 2 top monkeys";;

(* ========== PARSE INPUT ========== *)
(* Monkey 0:
     Starting items: 79, 98
     Operation: new = old * 19
     Test: divisible by 23
       If true: throw to monkey 2
       If false: throw to monkey 3 *)
exception ParseError of string;;
let failparse str =
    raise (ParseError str);;

let parse_items s =
    match Str.split (Str.regexp_string "Starting items: ") s with
    | h :: [] -> List.map int_of_string (Str.split (Str.regexp_string ", ") h)
    | _ -> failparse "parse_items";;

let parse_op s =
    match Str.split (Str.regexp_string "Operation: new = old ") s with
    | h :: [] -> match String.split_on_char ' ' h with
        | "*"::"old"::[] -> (fun old -> old * old)
        | "+"::s::[] -> let n = int_of_string s in (fun old -> old + n)
        | "*"::s::[] -> let n = int_of_string s in (fun old -> old * n)
        | s -> failparse @@ "parse_op: " ^ h
    | _ -> failparse "parse_op";;

let parse_decision = function
    | test :: iftrue :: iffalse :: [] ->
        let open Str in
        let div_by = match split (regexp_string "Test: divisible by ") test with
        | s :: [] -> int_of_string s
        | _ -> failparse "parse_decision"
        in
        let true_throw = match split (regexp_string "If true: throw to monkey ") iftrue with
        | s :: [] -> int_of_string s
        | _ -> failparse "parse_decision"
        in
        let false_throw = match split (regexp_string "If false: throw to monkey ") iffalse with
        | s :: [] -> int_of_string s
        | _ -> failparse "parse_decision"
        in
        mk_decis div_by true_throw false_throw
    | _ -> failparse "parse_decision";;

let parse_monkey = function
    | _ :: items :: op :: decis -> {
        items = parse_items items;
        op = parse_op op;
        decis = parse_decision decis 
};;

let lines = clean_lines (file_to_lines filename);;
let windows = (list_windowed 6 lines);;

(* ===== PART 1 ===== *)
let worry_lvl1 itm =
    quotient itm 3;;

printf "===== PART 1 =====\n%!";;
let monkeys1 = List.map parse_monkey windows;;
printf "----- Start monkeys -----\n%!";; 
List.iter print_monkey monkeys1;;

let part1 = solve worry_lvl1 20 monkeys1;;
printf "Part 1: %d\n%!" part1;;

(* ===== PART 2 ===== *)
printf "===== PART 2 =====\n%!";;
(* for part2 we need to collect divisible by %d numbers *)
let re = Str.regexp {|divisible by \([0-9]+\)|};;
let div_bys = List.map (fun w -> 
    let text = String.concat "" w in
    match re_grp 1 re text with
    | None -> failparse "parsing divisible by failed"
    | Some s -> int_of_string s)
    (list_windowed 6 lines);;

let lcm = list_lcm div_bys;;

let worry_lvl2 itm =
    itm mod lcm;;

let part2 = solve worry_lvl2 10000 @@ List.map parse_monkey windows;;
printf "Part 2: %d\n%!" part2;;
