open Aoc.Helpers

let filename = input_for_day 10

(* TODO optimize *)
let list_picki indexes lst =
    List.map (fun i -> (i, List.nth lst i)) indexes;;

let x_init = 1;;

(* c is a cycle counter, regs is a hash table of registers (name: value) *)
type 'a machine = { c: int; regs: (string, 'a) Hashtbl.t };;

let mk_machine regs_assoc_list = 
    let tbl = Hashtbl.create 1 in
    List.iter (fun (name, v) -> Hashtbl.replace tbl name v) regs_assoc_list;
    { c = 1; regs = tbl };;

let get_reg name {c; regs} =
    Hashtbl.find regs name;;

let set_reg name v machine =
    Hashtbl.replace machine.regs name v;
    machine;;

let map_reg name f machine =
    set_reg name (f (get_reg name machine)) machine;;

(* op consists of a cycle length and a procedure that accepts a machine 
   and returns a new machine *)
type 'a op = { len: int; proc: 'a machine -> 'a machine };;

let noop = { len=1; proc = Fun.id };;

let mk_addx n = { 
    len = 2; 
    proc = fun m -> map_reg "x" (fun x -> x + n) m 
};;

let do_op machine {len; proc} =
    let m = proc machine in
    { c = machine.c + len; regs = m.regs };;

let cpy_machine m =
    { c = m.c; regs = Hashtbl.copy m.regs };;

(* returns a list of machine states after each performed OP *)
let run ops machine =
    List.rev @@ List.fold_left 
        (fun res op -> match res with 
            | h :: t -> (do_op (cpy_machine h) op) :: res
            | [] -> assert false)
        [machine] ops;;

let machine_to_cycles start fin =
    List.map (fun i -> { c = i; regs = start.regs}) (range start.c fin.c);;

(* transform a list of machine states into a list of states for each cycle *)
let cycle_states machines = 
    let rec iter prev tail =
        match tail with
        | [] -> []
        | h :: t -> (machine_to_cycles prev h) :: (iter h t)
    in
    match machines with
    | [] -> []
    | h :: t -> iter h t;;

let sig_strength cycle x = cycle * x;;

(* PARSING INPUT *)
let line_to_op ln =
    match String.split_on_char ' ' ln with
    | "noop"::[] -> noop
    | "addx"::n::[] -> mk_addx (int_of_string n)
    | _ -> assert false;;

let lines = file_to_lines filename;;

let operations = 
    List.map line_to_op lines;;

let run_snapshots =
    run operations (mk_machine [("x", 1)]);;

let cycle_snapshots = 
    List.flatten @@ cycle_states run_snapshots;;

let x_values = List.map (get_reg "x") cycle_snapshots;;

let part1 = list_sum @@ List.map (fun (i, x) -> sig_strength (inc i) x) @@ 
    list_picki [19; 59; 99; 139; 179; 219] x_values;;

(* part 2 *)
let is_visible x px =
    Int.abs (x - px) <= 1;;

let print_row row =
    List.iter print_char row;
    print_newline();;

let pixels = List.mapi (fun i x -> if is_visible x i then '#' else '.') x_values;;
let rows = list_windowed 40 pixels;;
List.iter print_row rows;;
