(* The type checker runs through the AST, at each node check its
 * well-formedness. It uses the symbol table created in analyze
 * and if any semantic error found, it throws an exception back
 * to snick.ml, where it is catched. The codegen.ml will not be
 * executed.
 * 
 * Team Name - O'Harry the Caml:
 *            - Wilkins J Leong
 *            - Tuyen Quang Pham
 *            - Jorge Ricardo Pardave Garcia        *)

(* This file contains all the well-formedness checkers *)
open Symbol
open Snick_ast
open Printf


(* ================================================ *)
(* =====      Utility Functions         =========== *)
(* ================================================ *)

(* check if a symbol is in the scope *)
let check_symbol symbol scope = 
    let symbol_name = symbol.identifier in
    if not (List.exists (fun x -> x.identifier = symbol_name && x.scope = scope) Symbol.symbol_table.symbol_list) then
        failwith "Error: this symbol is not defined."

let check_symbol_id symbol_name scope = 
    (* print_string "Checking id ... "; *)
    (* Printf.printf "%s" symbol_name; *)
    (* print_string "\n"; *)
    if not (List.exists (fun x -> x.identifier = symbol_name && x.scope = scope) Symbol.symbol_table.symbol_list) then
        failwith "Error: this symbol is not defined."
    else
        ()
        (* print_string "Success !!\n" *);;
(*check if proc defined*)
let check_callid id = 
    if not (List.exists (fun x -> x.proc_name = id) proc_table.proc_list) then
        failwith "Error: this proc is not defined."


(* get the type of symbol *)
let rec match_symbol supproc id =
    check_symbol_id id supproc;
    let symbol = find_symbol id supproc in
    	symbol.sym_typespec   


(* calculate the type of lvalue *)
let rec match_lvalue supproc lvalue =
   match lvalue with 
   | LId id -> match_symbol supproc id
   | LField (id, exprs) -> match_symbol supproc id 	

(* calculate the type of an expression *)
let rec match_expr supproc expr =
    match expr with
     | Elval lval -> match_lvalue supproc lval
     | Const c -> match_const c
     | Ebinop (expr1, binop, expr2) -> match_binop supproc (expr1, binop, expr2)	   	
     | Eunop (unop, expr) -> match_unop supproc (unop, expr)
(*return constant type*)
and match_const con =
	match con with
	 | Ebool b -> "bool"
   | Eint i -> "int"
	 | Efloat f -> "float"
	 | EString st -> "string" 	
(* calculate the type of binop expr *)
and match_binop supproc (expr1, binop, expr2) =
    let expr1_type = match_expr supproc expr1 in
    let expr2_type = match_expr supproc expr2 in
    match binop with
    | Op_add | Op_sub | Op_mul | Op_div  -> 
      if (expr1_type = "bool") ||
	 (expr2_type = "bool") then
	 failwith "Error: operands types mismatch for arithmetic operator."
      else 
       begin
				 if (expr1_type = "float" or expr2_type = "float") then
					"float"
				 else
						"int"
       end	
    | Op_lt | Op_ltEq | Op_gt | Op_gtEq  ->
			if (expr1_type <> expr2_type) then
				if (expr1_type = "bool" ||
					 expr2_type = "bool"	) then
				 		failwith "Error: operands types mismatch for comparision operator."
				else
					"bool"
			else  	
		 		"bool"
    | Op_eq | Op_nEq ->
      if (expr1_type <> expr2_type) then
				failwith "Error: operands types mismatch for comparision operator."
      else
				"bool"
    | Op_or | Op_and ->
      if (expr1_type <> "bool"
				 || expr2_type <> "bool") then
 				failwith "Error: operands types mismatch for logical operator."
			else
				"bool"
				
(* calculate the type of unop expr *)
and match_unop supproc (unop, expr) =
    let expr_type = match_expr supproc expr in
    match unop with
	| Op_minus -> 
	  if expr_type = "bool" then
	    failwith "Error: operands types mismatch for unary minus."
	  else
	     expr_type	 
        | Op_not ->
	  if expr_type <> "bool" then
            failwith "Error: operands types mismatch for negation."
    else
    	expr_type

let btost b =
  if b then
    "true"
  else 
    "false"  
(* ================================================ *)
(* ========     Check Functions         =========== *)
(* ================================================ *)

(* check rvalue
 *      if rvalue is an expression --> call match_expr
 *       *)
let rec check_rvalue supproc type_name (Rexpr expr) =
    let right_type = match_expr supproc expr in 
                          if (type_name <> right_type) then
															if (type_name <> "float" &&
															    right_type <> "int")then 
                                failwith "Error: Assignment type mismatch." 
(*basic exp type*)
let match_basic_expr_type expr =
    match expr with
        | Const (Ebool _) -> "bool"
        | Const (Eint _) -> "int"
			  | Const (Efloat _) -> "float"
		    | Const (EString _) -> "string"
        | Elval _ -> "lval"
        | Ebinop _ -> "binop"
        | Eunop _ -> "unop"
      
                          
(* check each statement, matching with varies statement types *)
let rec check_stmt supproc stmt =
  match stmt with
  | Atomic astmt -> check_astmt supproc astmt
  | Composite cstmt -> check_cstmt supproc cstmt
(*check atomic stmt*)
and check_astmt supproc stmt = 
  match stmt with
  | Assign (lval,rval) -> check_assign supproc (lval,rval)
  | Read lval -> check_read supproc lval
  | Write expr -> check_write supproc expr
  | ProcCall (id, expr_lst) -> check_call supproc (id, expr_lst)
  (*check compound stmt*)
and check_cstmt supproc stmt =
  match stmt with
  | IfThenFi (expr, stmts) -> check_ifthen supproc (expr, stmts) 
  | IfThenElseFi (expr, stmts1, stmts2) -> check_ifthenelse supproc (expr, stmts1, stmts2)	  
  | WhileDo (expr, stmts) -> check_while supproc (expr, stmts)

(* check assignment: check lvalue's type match rvalue's type *)
and check_assign supproc (lvalue, rvalue) =
    let left_type = match_lvalue supproc lvalue in
    check_rvalue supproc left_type rvalue;
    ()	
	
(* check read statement: check the lvalue is bool/int or alias of these *)
and check_read supproc lvalue =
    let lvalue_type = match_lvalue supproc lvalue in
    ()

(* check write statement: check symbols in expr --> call check_expr *)
and check_write supproc expr =
    let type_name = match_expr supproc expr in
    match type_name with
        | _ -> ()

(* check call statement: 
 *      check if the proc name exists
 *      check if the params are in scope
 *      check if the params type and sequence *)
and check_call supproc (id, exprlst) =
    let proc_name = id in
    let param_list = find_all_params proc_name in
    let sorted_param_list = param_list in
    let call_num = List.length exprlst in
    let define_num = List.length param_list in
    check_callid proc_name; (* check proc name *)
    if call_num <> define_num then (* check param list length*)
        failwith "Error: Function call parameter number does not match.";
    for i = 0 to (call_num - 1) do (* check each param*)
        let define_type = (List.nth sorted_param_list i).sym_typespec in
        let call_type = match_expr supproc (List.nth exprlst i) in
        let define_pass = (List.nth sorted_param_list i).pass_by_ref in
        let call_pass = match_basic_expr_type (List.nth exprlst i) in
        if define_pass = true && call_pass <> "lval" then
          begin
            failwith "Error: Cannot pass non-address type value into ref parameter.";
           end 
        else
            if call_type <> define_type then
							 if (call_type <> "int" &&
									define_type <> "float") then
                failwith "Error: Function call with wrong param types.";
    done
    

    
(* check `if then` statement:
 *      check if `if` expr is bool
 *      check `then` block --> recursive call check_stmt *)
and check_ifthen supproc (expr, stmtlst) = 
    let expr_type = match_expr supproc expr in
    if not(expr_type = "bool") then
        failwith "Error: if statement can only take bool type.";
    List.iter (fun x -> check_stmt supproc x) stmtlst

(* check `if then else` statement:
 *      check if `if` expr is bool
 *      check `then` block --> recursive call check_stmt
 *      check `else` block --> recursive call check_stmt *)
and check_ifthenelse supproc (expr, then_stmtlst, else_stmtlst) = 
    let expr_type = match_expr supproc expr in
    if not(expr_type = "bool") then
        failwith "Error: if statement can only take bool type.";
    List.iter (fun x -> check_stmt supproc x) then_stmtlst;
    List.iter (fun x -> check_stmt supproc x) else_stmtlst

(* check `while` statement:
 *      check `while` expr is bool
 *      check `do` block --> recursive call check_stmt *)
and check_while supproc (expr, stmtlst) = 
    let expr_type = match_expr supproc expr in
    (*printf "%s\n" expr_type;*)
    if not(expr_type = "bool") then
        failwith "Error: while statement can only take bool type.";
    List.iter (fun x -> check_stmt supproc x) stmtlst

(* Check the procbody, iter through all statements *)
let check_procbody procname (decls, stmts) =
    List.iter (check_stmt procname) stmts

(* Check procs *)
let check_proc proc = 
    let (ProcHead (id,params), ProcBody (decls, stmts)) = 
      (proc.head, proc.body) in
    check_procbody id (decls, stmts)

(* Entry point: given the program go check all its children *)
let check_program program =
    List.iter check_proc program.procs
