open Aoc.Helpers
open Printf

module Mathexpr = struct
    type t =
        | Const of int
        | Var of string
        | Add of t * t
        | Sub of t * t
        | Mul of t * t
        | Div of t * t

    let rec to_str = function
        | Const c -> string_of_int c
        | Var s -> s
        | Add(e1, e2) -> sprintf "(%s + %s)" (to_str e1) (to_str e2)
        | Sub(e1, e2) -> sprintf "(%s - %s)" (to_str e1) (to_str e2)
        | Mul(e1, e2) -> sprintf "(%s * %s)" (to_str e1) (to_str e2)
        | Div(e1, e2) -> sprintf "(%s / %s)" (to_str e1) (to_str e2)

    let rec has_var name = function
        | Var s when s = name -> true
        | Var _ -> false
        | Const _ -> false
        | Add(e1, e2) -> has_var name e1 || has_var name e2
        | Sub(e1, e2) -> has_var name e1 || has_var name e2
        | Mul(e1, e2) -> has_var name e1 || has_var name e2
        | Div(e1, e2) -> has_var name e1 || has_var name e2

    let rec solve_eq var_name lhs rhs =
        (* x + c = y -> x = y - c *)
        let do_add e1 e2 =
            if has_var var_name e1
            then solve_eq var_name e1 (Sub(rhs, e2))
            else solve_eq var_name e2 (Sub(rhs, e1))
        in
        (* x - c = y -> x = y + c *)
        (* c - x = y -> x = c - y *)
        let do_sub e1 e2 =
            if has_var var_name e1
            then solve_eq var_name e1 (Add(rhs, e2))
            else solve_eq var_name e2 (Sub(e1, rhs))
        in
        (* x * c = y -> x = y / c *)
        let do_mul e1 e2 =
            if has_var var_name e1
            then solve_eq var_name e1 (Div(rhs, e2))
            else solve_eq var_name e2 (Div(rhs, e1))
        in
        (* x / c = y -> x = y * c *)
        (* c / x = y -> x = c / y *)
        let do_div e1 e2 =
            if has_var var_name e1
            then solve_eq var_name e1 (Mul(rhs, e2))
            else solve_eq var_name e2 (Div(e1, rhs))
        in
        match lhs with
        | Var s when s = var_name -> rhs
        | Add(e1, e2) -> do_add e1 e2
        | Sub(e1, e2) -> do_sub e1 e2
        | Mul(e1, e2) -> do_mul e1 e2
        | Div(e1, e2) -> do_div e1 e2
        | expr -> failwith (sprintf "resulting lhs was %s" (to_str expr))

    let rec to_int = function
        | Var _ -> failwith "can't reduce a variable"
        | Const c -> c
        | Add(e1, e2) -> (to_int e1) + (to_int e2)
        | Sub(e1, e2) -> (to_int e1) - (to_int e2)
        | Mul(e1, e2) -> (to_int e1) * (to_int e2)
        | Div(e1, e2) -> (to_int e1) / (to_int e2)
end

module Env = struct
    type op_t = Add | Sub | Mul | Div

    type rhs_t =
        | Var
        | Const of int
        | Expr of (string * string) * op_t

    type def_t = {
        lhs: string;
        rhs: rhs_t
    }

    let mk_def lhs rhs =
        { lhs = lhs; rhs = rhs }

    let def_rhs def =
        def.rhs

    let def_lhs def =
        def.lhs

    let mk_expr name_pair op =
        Expr(name_pair, op)

    type 'a defs_t = { tbl: (string, rhs_t) Hashtbl.t }

    let mk_defs def_lst =
        let tbl = Hashtbl.create (List.length def_lst) in
        List.iter (fun { lhs; rhs } -> Hashtbl.replace tbl lhs rhs) def_lst;
        { tbl = tbl }

    let lookup defs name =
        Hashtbl.find defs.tbl name

    let rec mathexpr defs name =
        let rhs = lookup defs name in
        match rhs with
        | Var -> Mathexpr.Var name
        | Const c -> Mathexpr.Const c
        | Expr((s1, s2), op) -> begin
            match op with
            | Add -> Mathexpr.Add(mathexpr defs s1, mathexpr defs s2)
            | Sub -> Mathexpr.Sub(mathexpr defs s1, mathexpr defs s2)
            | Mul -> Mathexpr.Mul(mathexpr defs s1, mathexpr defs s2)
            | Div -> Mathexpr.Div(mathexpr defs s1, mathexpr defs s2)
        end

end

(* ---------- PARSING INPUT ---------- *)

let mk_add s1 s2 =
    Env.mk_expr (s1,s2) Env.Add

let mk_mul s1 s2 =
    Env.mk_expr (s1,s2) Env.Mul

let mk_sub s1 s2 =
    Env.mk_expr (s1,s2) Env.Sub

let mk_div s1 s2 =
    Env.mk_expr (s1,s2) Env.Div

let parse_line ln =
    (*print_endline ln;*)
    let parse_const s d =
        Env.mk_def s (Const d)
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
        Env.mk_def s (mkf arg1 arg2)
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
    let parsed = List.map parse_line (file_to_lines fname) in
    let defs = Env.mk_defs parsed in
    Env.mathexpr defs "root" |> Mathexpr.to_int

let test_s1 = part1 (test_input_for_day 21)
let _ = printf "Part 1 test: %d\n" test_s1
let s1 = part1 (input_for_day 21)
let _ = printf "Part 1: %d\n" s1

let part2 fname =
    let parsed = List.map parse_line (file_to_lines fname)
            |> list_replace (fun def -> (Env.def_lhs def = "humn"))
                            (fun def -> Env.mk_def (Env.def_lhs def) Var)
    in
    let defs = Env.mk_defs parsed in
    let root_rhs = Env.lookup defs "root" in
    let rhs_op1, rhs_op2 = begin
        match root_rhs with
        | Env.Expr((s1, s2), _) -> (s1, s2)
        | _ -> failwith "wtf"
    end in
    let lhs = Env.mathexpr defs rhs_op1 in
    let rhs = Env.mathexpr defs rhs_op2 in
    let sol_rhs = Mathexpr.solve_eq "humn" lhs rhs in
    Mathexpr.to_int sol_rhs

let test_s2 = part2 (test_input_for_day 21)
let _ = printf "Part 2 test: %d\n" test_s2
let s2 = part2 (input_for_day 21)
let _ = printf "Part 2: %d\n" s2
