open Aoc.Helpers
open Printf

(* We need to transform a recursive evaluation into an iterative one *)
(* Example:
    ((a + b) * (c / d)) + ((e - f) / (g + h))
   Recursive evaluation:
    ((a + b) * (c / d)) + ((e - f) / (g + h))
        ((a + b) * (c / d))   
            (a + b)
            (c / d)
        <-
        ((e - f) / (g + h))
            (e - f)
            (g + h)
        <-
    <-
    Iterative evaluation:
    iter l1 r1 [+]
    iter ? ? [*;+]
    iter ? ? [+;*;+]
    iter ? ? [/;*;+]
    *)

module Env = struct
    type 'a def_t = {
        name: string;
        expr: 'a expr_t
    }
    and 'a expr_t =
        | Const of 'a
        | Expr of string list * ('a list -> 'a)

    let mk_const x =
        Const x

    let mk_expr names f =
        Expr(names, f)

    type 'a defs_t = { tbl: (string, 'a expr_t) Hashtbl.t }

    let mk_defs pairs =
        let tbl = Hashtbl.create (List.length pairs) in
        List.iter (fun (name, expr) -> Hashtbl.replace tbl name expr) pairs;
        { tbl = tbl }

    let lookup defs name =
        Hashtbl.find defs.tbl name
     
    let rec get defs name =
        match lookup defs name with
        | Const x -> x
        | Expr(names, f) -> f (List.map (get defs) names)
end

(* ---------- PARSING INPUT ---------- *)
let mk_bifun s1 s2 f =
    Env.mk_expr [s1; s2] (fun [v1;v2] -> f v1 v2)

let mk_add s1 s2 =
    mk_bifun s1 s2 (+)

let mk_mul s1 s2 =
    mk_bifun s1 s2 ( * )

let mk_sub s1 s2 =
    mk_bifun s1 s2 (-)

let mk_div s1 s2 =
    mk_bifun s1 s2 (/)

let parse_line ln =
    (*print_endline ln;*)
    let parse_const s d =
        (s, Env.mk_const d)
    in
    let parse_expr s arg1 op arg2 =
        let mkf = begin
            match op with
            | "+" -> mk_add
            | "-" -> mk_sub
            | "*" -> mk_mul
            | "/" -> mk_div
            | s -> failwith (sprintf "unknown operator %s" s)
        end in
        (s, mkf arg1 arg2)
    in
    let name = String.sub ln 0 4 in
    let ln_rest = (str_from 6 ln) in
    let len = (List.length (String.split_on_char ' ' ln_rest)) in
    if len < 1
    then failwith (sprintf "parse_line: failed on %s" ln)
    else if len = 1 then Scanf.sscanf ln_rest "%d" (parse_const name)
    else Scanf.sscanf ln_rest "%s %s %s" (parse_expr name)
(* ---------- DONE PARSING INPUT ---------- *)

let part1 fname =
    let pairs = List.map parse_line (file_to_lines fname) in
    let defs = Env.mk_defs pairs in
    let root = Env.get defs "root" in
    root

let test_s1 = part1 (test_input_for_day 21)
let _ = printf "Part 1 test: %d\n" test_s1
let s1 = part1 (input_for_day 21)
let _ = printf "Part 1 test: %d\n" s1
