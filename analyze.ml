(* ===========================================================================*)
(* Symbol table analyze and generation for the snick compiler                 *)
(* ---------------------------------------------------------------------------*)
(* It reads the syntax tree after parser and analyze the tree to generate     *)
(* symbol table                                                               *)
(* FileName: analyze.ml                                                       *)
(* Team Name - O'Harry the Caml:
 *            - Wilkins J Leong
 *            - Tuyen Quang Pham
 *            - Jorge Ricardo Pardave Garcia *)                                                       
(* ===========================================================================*)

open Symbol
open Snick_ast
open Printf
(* used to count the slot number in proc, reset to 0 once proc end *)
let proc_slot_count=ref 0

    

(*build symbol table for single array decl*)
let rec alyz_subfield supproc ref_flag param_flag (id,typespec)=
    match typespec with
      | Bool-> Symbol.add_symbol {identifier=id; slot_start = !proc_slot_count; 
                                  slot_end = !proc_slot_count ; 
                                  array_dim = [];sym_typespec="bool";
                                  scope=supproc; pass_by_ref=ref_flag;
                                  param=param_flag};
              proc_slot_count := !proc_slot_count+1
      | Int -> Symbol.add_symbol {identifier=id; slot_start = !proc_slot_count;
                                  slot_end = !proc_slot_count; 
                                  array_dim = [];sym_typespec="int";
                                  scope=supproc; pass_by_ref=ref_flag;
                                  param=param_flag};
              proc_slot_count := !proc_slot_count+1
      | Float -> Symbol.add_symbol {identifier=id; slot_start = !proc_slot_count;
                                  slot_end = !proc_slot_count; 
                                  array_dim = []; sym_typespec="float";
                                  scope=supproc; pass_by_ref=ref_flag;
                                  param=param_flag};
              proc_slot_count := !proc_slot_count+1        
      
(*build sym table for array decl*)    
let alyz_array_decl supproc ref_flag param_flag (typespec,id,intervals) =
  let slot_num = 
    (List.fold_left (fun acc (Interval (head, tail)) -> acc * (tail - head + 1)) 1 intervals) in
    (*printf "%d\n" slot_num;*)
  let interval_list =
                (List.map (fun (Interval inter) -> inter) intervals) in
      match typespec with 
      |Bool-> 
        Symbol.add_symbol {identifier=id; slot_start = !proc_slot_count; 
                           slot_end= !proc_slot_count + slot_num - 1; 
                           array_dim = interval_list;
                           sym_typespec="bool";scope=supproc; 
                           pass_by_ref=ref_flag; param=param_flag} ;
        proc_slot_count := !proc_slot_count+slot_num

      |Int -> Symbol.add_symbol {identifier=id; slot_start= !proc_slot_count; 
                                  slot_end= !proc_slot_count + slot_num - 1; 
                                  array_dim=interval_list; sym_typespec="int";
                                  scope=supproc; pass_by_ref=ref_flag;
                                  param=param_flag};
              proc_slot_count := !proc_slot_count+slot_num

      |Float -> Symbol.add_symbol {identifier=id; slot_start= !proc_slot_count; 
                                  slot_end= !proc_slot_count + slot_num - 1; 
                                  array_dim=interval_list;sym_typespec="float";
                                  scope=supproc; pass_by_ref=ref_flag;
                                  param=param_flag};
              proc_slot_count := !proc_slot_count+slot_num
(* analyze proc parameters *)
let alyz_parameter supproc (Param (passspec,typespec,ident))=
    let ref_flag=(match passspec with IndiVal ->  false | IndiRef ->  true) in
      alyz_subfield supproc ref_flag true (ident,typespec)

(* analyze proc *)
let alyz_procheader (procname, params)=
    List.iter (alyz_parameter procname) params

(* analyze proc variable declation *)
let alyz_vardecl supproc decl =
  match decl with
  | DeclVar (typespec, id) ->  
      alyz_subfield supproc false false (id,typespec)
  | DecArr (typespec, id, intervals) ->
      alyz_array_decl supproc false false (typespec,id,intervals)       
    
(* analyze proc body  *)
let alyz_procbody procname (decls, stmts)=
    List.iter (alyz_vardecl procname) decls
 (*analyze proc*)       
let alyz_proc proc =
    let (ProcHead (procname, params), ProcBody (decls, stmts))=(proc.head, proc.body) in
      proc_slot_count := 0;
      alyz_procheader (procname, params);
      alyz_procbody procname (decls, stmts);
      Symbol.add_proc {proc_name=procname; 
                       proc_size=(Symbol.calc_size_proc procname)}
(*analyze program*)
let alyz_program program = 
    Symbol.init();
    List.iter alyz_proc program.procs

(* a function to show current current symbol table *)
let show_table () =
    print_string "\n======symbol table=======\n";
    Symbol.print_symbol_list Symbol.symbol_table.symbol_list;
    print_string "\n======proc table=======\n";
    Symbol.print_proc_list Symbol.proc_table.proc_list;
