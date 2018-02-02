(* ===========================================================================*)
(* Symbol table structure with access and util interface                      *)
(* ---------------------------------------------------------------------------*)
(* It define the table structure for various usage and implement the basic    *)
(* data and util function toward each table                                   *)
(* FileName: symbol.ml                                                        *)
(* Author: Team Name - O'Harry the Caml: 
 *            - Wilkins J Leong
 *            - Tuyen Quang Pham
 *            - Jorge Ricardo Pardave Garcia *)	                                                          
(* ===========================================================================*)

(* terminal symbol var or array name*)
type symbol = {
    identifier:string; 
    slot_start:int;
    slot_end:int; 
    array_dim : (int * int) list;
    sym_typespec:string; 
    scope:string;  
    pass_by_ref:bool; 
    param:bool 
}
(*proc symbol table*)
type proc = {
    proc_name:string;
    proc_size:int
}

type symbol_stack = {mutable symbol_list: symbol list}
type proc_stack = {mutable proc_list : proc list}

let symbol_not_found = {identifier = "not_found_404"; slot_start = 0; 
                        slot_end = 0; array_dim = []; sym_typespec = "not_found"; 
                        scope = "not_found"; pass_by_ref = false; 
                        param = false}

let proc_not_found = {proc_name = "not_found_404"; proc_size = 0 }

let symbol_table = {symbol_list = []}
let proc_table = {proc_list = []}
(*the next 4 function look for an arry or var in a scope*)
let find_symbol id scope =
    try (List.find (fun s->s.identifier=id && s.scope=scope) 
        symbol_table.symbol_list) with Not_found -> symbol_not_found

let find_symbol_by_slot slot_start slot_end scope =
    try (List.find (fun s-> s.slot_start=slot_start && s.slot_end=slot_end && s.scope=scope) 
        symbol_table.symbol_list) with Not_found -> symbol_not_found

let find_symbol_instance symbol scope =
    let id = symbol.identifier in
    let scope_name = scope.proc_name in
    try (List.find (fun s->s.identifier=id && s.scope=scope_name) 
        symbol_table.symbol_list) with Not_found -> symbol_not_found
(*find a proc in table*)
let find_proc id =
    try (List.find (fun s->s.proc_name=id) proc_table.proc_list) 
    with Not_found -> proc_not_found
(*add var or arry to table*)
let add_symbol x =
    if not (List.exists (fun s->s.identifier=x.identifier && s.scope=x.scope) 
        symbol_table.symbol_list) then
        symbol_table.symbol_list <- symbol_table.symbol_list@[x]
    else
        failwith "Error: symbol declaration duplicated."
(*add proc to table*)
let add_proc x =
    if not (List.exists (fun s->s.proc_name=x.proc_name) 
        proc_table.proc_list) then
        proc_table.proc_list <- proc_table.proc_list@[x]

(* init function *)
let init ()=
    symbol_table.symbol_list <- [];
    proc_table.proc_list <- []

(* find all symbol in the proc  *)
let find_all_symbol proc_name = 
    List.filter (fun x -> x.scope = proc_name) symbol_table.symbol_list 
(*find all param in proc*)
let find_all_params proc_name = 
    List.filter (fun x -> x.scope = proc_name && x.param = true)
    symbol_table.symbol_list 
(*return stack frame size for proc*)
let calc_size_proc proc_name = 
    let allsym = find_all_symbol proc_name in
    	let slotnum = 
    		List.map (fun x -> x.slot_end - x.slot_start + 1) allsym
    	in
    		List.fold_left (fun acc x -> acc + x) 0 slotnum 	

(*util function for debug*)
let b2s b=
  if b then "true" else "false"

let print_symbol_list slst=
    List.iter (fun x -> print_string (x.identifier^" "^(string_of_int x.slot_start)
        ^" "^(string_of_int x.slot_end)^" "^x.sym_typespec^" "^x.scope^" "^(b2s x.pass_by_ref)^"--\n")) slst
let print_proc_list plst=
    List.iter (fun x -> print_string (x.proc_name^" "^
        (string_of_int x.proc_size)^"--\n")) plst  
