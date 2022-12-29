open Aoc.Helpers
open Printf

module type DLList_t = sig
    type 'a t
    type 'a node_t
    val nodes_of_list : 'a list -> 'a node_t list
    (*val to_list : 'a t -> 'a list*)
    val link : 'a node_t -> 'a node_t -> unit
    val fwd : int -> 'a node_t -> 'a node_t
    val bwd : int -> 'a node_t -> 'a node_t
    val v : 'a node_t -> 'a
    val prev : 'a node_t -> 'a node_t
    val next : 'a node_t -> 'a node_t
    val take : int -> 'a node_t -> 'a node_t list
    val node_to_str : ('a -> string) -> 'a node_t -> string
end

module DLList : DLList_t = struct
    type 'a node_t = {
        v : 'a;
        mutable next : 'a node_t option;
        mutable prev : 'a node_t option;
    }

    type 'a t =
        | Nil
        | Node of int * 'a node_t

    let mk_t v next prev =
        { v = v; next = next; prev = prev }

    let v node =
        node.v

    let next node =
        Option.get node.next

    let prev node =
        Option.get node.prev

    let node_to_str v_to_str node =
        sprintf "%s - %s - %s" (v_to_str (prev node).v) (v_to_str node.v) (v_to_str (next node).v)

    let link left right =
        left.next <- Some right;
        right.prev <- Some left;
        ()

    let rec fwd n node =
        if n = 0 then node
        else fwd (n - 1) (next node)

    let rec bwd n node =
        if n = 0 then node
        else bwd (n - 1) (prev node)

    let nodes_of_list lst =
        let rec iter prev acc = function
            | [] -> acc
            | hd :: tl ->
                let node = mk_t hd None None in
                link prev node;
                iter node (node :: acc) tl
        in
        let nodes = begin
            match lst with
            | [] -> []
            | hd :: tl ->
                let hd_node = (mk_t hd None None) in
                iter hd_node [hd_node] tl |> List.rev
        end in
        link (list_last nodes) (List.hd nodes);
        nodes

    let take n node =
        let rec iter i acc curr =
            if i = n then acc
            else iter (i + 1) (curr :: acc) (prev curr)
        in
        iter 0 [] node
end

let move_fwd n node =
    let v, prev, next = DLList.v node, DLList.prev node, DLList.next node in
    let new_prev = DLList.fwd n node in
    DLList.link prev next;
    DLList.link node (DLList.next new_prev);
    DLList.link new_prev node;
    ()

let move_bwd n node =
    let v, prev, next = DLList.v node, DLList.prev node, DLList.next node in
    let new_next = DLList.bwd n node in
    DLList.link prev next;
    DLList.link (DLList.prev new_next) node;
    DLList.link node new_next;
    ()

let move len node =
    let v = (DLList.v node) in
    if v = 0 then ()
    else begin
        let move_f = if v > 0 then move_fwd else move_bwd in
        let n = (Int.abs v) mod (len - 1) in
        move_f n node
    end

let mix_nums n nums =
    let len = List.length nums in
    let nodes = DLList.nodes_of_list nums in
    for _ = 1 to n do
        List.iter (move len) nodes
    done;
    DLList.take len (List.hd nodes) |> List.map DLList.v

let parse_nums input =
    String.split_on_char '\n' input |> List.filter (Fun.negate string_empty)
    |> List.map int_of_string

let arr_index_of_opt x arr =
    let len = Array.length arr in
    let rec iter i =
        if i = len then None
        else if arr.(i) = x then Some i
        else iter (i + 1)
    in
    iter 0

let arr_index_of x arr =
    match arr_index_of_opt x arr with
    | None -> failwith "arr_index_of: not found"
    | Some i -> i

let part1 input =
    let nums = parse_nums input in
    let mixed_nums = mix_nums 1 nums |> Array.of_list in
    print_array print_int mixed_nums;
    let idx = arr_index_of 0 mixed_nums in
    let len = Array.length mixed_nums in
    List.map (fun n -> Array.get mixed_nums ((n + idx) mod len)) [1000; 2000; 3000]
    |> list_sum

let part2 input =
    let key = 811589153 in
    let nums = parse_nums input |> List.map (fun num -> num * key) in
    let mixed_nums = mix_nums 10 nums |> Array.of_list in
    (*print_array print_int mixed_nums;*)
    let idx = arr_index_of 0 mixed_nums in
    let len = Array.length mixed_nums in
    List.map (fun n -> Array.get mixed_nums ((n + idx) mod len)) [1000; 2000; 3000]
    |> list_sum


let test_input = file_to_text (test_input_for_day 20)
let input = file_to_text (input_for_day 20)

let s1_test = part1 test_input
let _ = printf "Part 1 test: %d\n" s1_test
let s1 = part1 input
let _ = printf "Part 1: %d\n" s1

let s2_test = part2 test_input
let _ = printf "Part 2 test: %d\n" s2_test
let s2 = part2 input
let _ = printf "Part 2: %d\n" s2
