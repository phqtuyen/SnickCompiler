(* ============================================= *)
(* Code generator for the snick compiler          *)
(* --------------------------------------------- *)
(* It reads the syntax tree from previous step   *)
(* and outputs the brill instructions in stdout  *)
(* Team Name - O'Harry the Caml:
 *            - Wilkins J Leong
 *            - Tuyen Quang Pham
 *            - Jorge Ricardo Pardave Garcia    *)                          
(* ============================================= *)

open Snick_ast
open Format
open Symbol
open Type_checking

(* labelnum counts the number of current label *)
(* regparam counts the number of register for following two reasons *)
(* load value or address of actual parameter to register *)
(* store value of formal parameter from register to slot *)
let labelnum = ref 0
let regparam = ref 0

(* generate code for Bool constant *)
let cg_bool fmt b reg = 
    if b then
        (fprintf fmt "int_const r%d, 1\n" reg)
    else
        (fprintf fmt "int_const r%d, 0\n" reg)

(* generate code for Integer constant *)
let cg_int fmt i reg = 
    fprintf fmt "int_const r%d, %d\n" reg i

(*generate code for float const*)
let cg_float fmt i reg = 
    fprintf fmt "real_const r%d, %f\n" reg i

(*generate code for string const*)
let cg_string fmt st = 
		fprintf fmt "string_const r0, %s\n" st


(* generate code for unary minus expression *)
let cg_uminus fmt (unop, expr) reg scope = 
    let reg1 = reg + 1 in
		let exp_type = Type_checking.match_expr scope expr in
		match exp_type with
		| "int" ->  
			fprintf fmt "int_const r%d, 0\n" reg1;		
	       fprintf fmt "sub_int r%d, r%d, r%d\n" reg reg1 reg
		| "float" ->
			fprintf fmt "real_const r%d, 0.0\n" reg1;
			fprintf fmt "sub_real r%d, r%d, r%d\n" reg reg1 reg
(*convert operator to string*)			
let op_to_st op = 
	match op with
	| Op_add -> "add"
	| Op_sub -> "sub"
	| Op_mul -> "mul"
	| Op_div -> "div" 
	| Op_eq -> "cmp_eq"
	| Op_nEq -> "cmp_ne"
	| Op_lt -> "cmp_lt"
	| Op_ltEq -> "cmp_le"
	| Op_gt -> "cmp_gt"
	| Op_gtEq -> "cmp_ge" 

(* generate code for each expression *)
let rec cg_expr fmt expr scope reg = 
		match expr with
		| Elval lval -> cg_lvalue_load fmt lval scope reg
		| Ebinop (expr1, binop, expr2) 
            -> cg_binop fmt (expr1,binop,expr2) scope reg
		| Eunop (unop, expr) -> cg_unop fmt (unop, expr) scope reg
		| Const (Eint i) -> cg_int fmt i reg
		| Const (Efloat f) -> cg_float fmt f reg
		| Const (Ebool b) -> cg_bool fmt b reg
		| Const (EString s) -> cg_string fmt s	

(* generate code for binary expression *)
and cg_binop fmt (expr1, binop, expr2) scope reg =
    let reg1 = reg + 1 in
	let type1 = Type_checking.match_expr scope expr1 in
	let type2 = Type_checking.match_expr scope expr2 in
            cg_expr fmt expr1 scope reg;
            cg_expr fmt expr2 scope reg1;
            (if binop = Op_div then
                match  type2 with
                | "int" ->
                    (*fprintf fmt "div0test:\n";*)
                    fprintf fmt "int_const r%d, 0\n" (reg1 + 1);
                    fprintf fmt "cmp_eq_int r%d, r%d, r%d\n" (reg1 + 1) (reg1 + 1) reg1;
                    fprintf fmt "branch_on_true r%d, runerror\n" (reg1 + 1)   
                | "float" ->
                    (*fprintf fmt "div0test:\n";*)
                    fprintf fmt "real_const r%d, 0.0\n" (reg1 + 1);
                    fprintf fmt "cmp_eq_real r%d, r%d, r%d\n" (reg1 + 1) (reg1 + 1) reg1;
                    fprintf fmt "branch_on_true r%d, runerror\n" (reg1 + 1)
             );      
            match binop with
                | Op_add | Op_sub | Op_mul | Op_div
        		| Op_lt | Op_gt | Op_ltEq | Op_gtEq -> 
        					if type1 = "int" &&
        						type2 = "int" then
        						fprintf fmt "%s_int r%d, r%d, r%d\n" 
                                    (op_to_st binop) reg reg reg1	 						 						
                            else			
        					begin
        						if type1 = "int" &&
        							type2 = "float" then
        							fprintf fmt "int_to_real r%d, r%d\n"
                                     reg reg;
        						if type1 = "float" &&
        							type2 = "int" then
        							fprintf fmt "int_to_real r%d, r%d\n"
                                        reg1 reg1;
        						fprintf fmt "%s_real r%d, r%d, r%d\n" 
                                    (op_to_st binop) reg reg reg1
        					end
        				
                | Op_eq | Op_nEq->
        					if (type1 = "int" || type1 = "bool") then	 
        						fprintf fmt "%s_int r%d, r%d, r%d\n" 
                                    (op_to_st binop) reg reg reg1
        					else
        						fprintf fmt "%s_real r%d, r%d, r%d\n" 
                                    (op_to_st binop) reg reg reg1
                
                | Op_and-> fprintf fmt "and r%d, r%d, r%d\n" reg reg reg1
                | Op_or-> fprintf fmt "or r%d, r%d, r%d\n" reg reg reg1

(* generate code for unary expression *)
and cg_unop fmt (unop, expr) scope reg = 
    cg_expr fmt expr scope reg;
    match unop with
        | Op_minus-> cg_uminus fmt (unop, expr) reg scope
        | Op_not -> fprintf fmt "not r%d, r%d\n" reg reg

(* generate code to load value or address of lvalue from slot to register *)
and cg_lvalue_load fmt lval scope reg = 
    match lval with 
    | LId id -> (*var*)
        let lsym = (Symbol.find_symbol id scope) in
        let lslot = lsym.slot_start in
        if lsym.pass_by_ref then 
            begin
            fprintf fmt "load r%d, %d\n" reg lslot;
            fprintf fmt "load_indirect r%d, r%d\n" reg reg
            end
        else
            (fprintf fmt "load r%d, %d\n" reg lslot)
    | LField (id, exprs)->  (*array*)
        let lsym = (Symbol.find_symbol id scope) in 
        let lslot = lsym.slot_start in
        if lsym.pass_by_ref then
            begin
                 ()        
            end
        else
            begin
                cg_array fmt (id, exprs) scope reg;
                fprintf fmt "load_address r%d, %d\n" (reg+1) lslot;
                fprintf fmt "sub_offset r%d, r%d, r%d\n" (reg+1) (reg+1) reg;
                fprintf fmt "load_indirect r%d, r%d\n" reg (reg+1)
            end    
(*calculate array address offset*)
and cg_array fmt (id, exprs) scope reg =
    let arry_sym = (Symbol.find_symbol id scope) in
    let (reg1, reg2, reg3, reg4)=(reg + 1, reg + 2, reg+3, reg+4) in
        cg_expr fmt (List.nth exprs 0) scope reg;
        let (l,h) = (List.nth arry_sym.array_dim 0) in
            cg_int fmt l reg1;   
            let no_exprs = (List.length exprs) in
                (for i = 1 to (no_exprs - 1) do
                    (*reg_ref := !reg_ref + 1;*)
                    let (low, high) = (List.nth arry_sym.array_dim i) in
                    let temp = high - low + 1 in
                        cg_int fmt temp reg2;
                        cg_int fmt low reg3; 
                        cg_expr fmt (List.nth exprs i) scope reg4;
                        fprintf fmt "mul_int r%d, r%d, r%d\n" reg reg reg2;
                        fprintf fmt "add_int r%d, r%d, r%d\n" reg reg reg4;
                        fprintf fmt "mul_int r%d, r%d, r%d\n" reg1 reg1 reg2;
                        fprintf fmt "add_int r%d, r%d, r%d\n" reg1 reg1 reg3;     
                done);
                fprintf fmt "sub_int r%d, r%d, r%d\n" reg reg reg1     

        
        

(* generate code to store lvalue *)
(* store value from register to slot or corresponding address *)
let cg_lvalue_store fmt lval scope = 
    match lval with 
    | LId id -> 
        let lsym = (Symbol.find_symbol id scope) in
        let lslot = lsym.slot_start in
        if lsym.pass_by_ref then (
            fprintf fmt "load r1, %d\n" lslot;
            fprintf fmt "store_indirect r1, r0\n"
        )
        else
            (fprintf fmt "store %d, r0\n" lslot)
    | LField (id, exprs) -> 
        let lsym = (Symbol.find_symbol id scope) in 
        let lslot = lsym.slot_start in
            if lsym.pass_by_ref then
                begin
                    ()
                end
            else
                begin
                    cg_array fmt (id, exprs) scope 1;
                    fprintf fmt "load_address r2, %d\n" lslot;
                    fprintf fmt "sub_offset r1, r2, r1\n";
                    fprintf fmt "store_indirect r1, r0\n";  
                end
(* generate code for assignment statement *)
let rec cg_assign fmt lval rval scope =
    let name = (match lval with
    | LId id -> id 
    | LField (id, exprs)-> id) in

        let ltype = (Symbol.find_symbol name scope).sym_typespec in
        (match rval with
            | Rexpr(expr) -> 
                cg_expr fmt expr scope 0;
                let rtype = match_expr scope expr in 
                    if ltype = "float" &&
                        rtype = "int" then
                        fprintf fmt "int_to_real r0, r0\n"; 
                    cg_lvalue_store fmt lval scope
            | _ -> ())      
 
(* generate code for read statement *)
let cg_read fmt lvalue scope = 
    let lvaluetype = Type_checking.match_lvalue scope lvalue in
    fprintf fmt "# read\n";
    (match lvaluetype with
        | "bool" -> fprintf fmt "call_builtin read_bool\n"
        | "int" -> fprintf fmt "call_builtin read_int\n"
        | "float" -> fprintf fmt "call_builtin read_real\n");
    cg_lvalue_store fmt lvalue scope

(* generate code for write except string statement *)
let cg_write fmt wexpr scope = 
    fprintf fmt "# write\n";
    cg_expr fmt wexpr scope 0;
    let exprtype = Type_checking.match_expr scope wexpr in
    match exprtype with
        | "bool" -> fprintf fmt "call_builtin print_bool\n"
        | "int" -> fprintf fmt "call_builtin print_int\n"
        | "float" -> fprintf fmt "call_builtin print_real\n"
        | "string" -> fprintf fmt "call_builtin print_string\n"
        | _ -> ()

(* generate code for each actual parameter *)
(* load value or address of actual parameter from slot to register *)
let cg_parameter_load fmt procname expr scope = 
    match expr with
        | Elval lval ->
            (match lval with
                | LId id ->
                  let rsym = Symbol.find_symbol id scope in
                  let rslot = rsym.slot_start in
                  let lsym = 
                    (Symbol.find_symbol_by_slot !regparam !regparam procname) in
                    if rsym.pass_by_ref then (
                            fprintf fmt "load r%d, %d\n" !regparam rslot;
                            (if rsym.sym_typespec = "int" &&
                                lsym.sym_typespec = "float" then
                                fprintf fmt "int_to_real r%d, r%d\n" !regparam !regparam);
                            regparam := !regparam + 1
                    )
                    else (
                        if lsym.pass_by_ref then (
                            (if rsym.sym_typespec = "int" &&
                                lsym.sym_typespec = "float" then
                                begin
                                    fprintf fmt "load r%d, %d\n" !regparam rslot;
                                    fprintf fmt "int_to_real r%d, r%d\n" !regparam !regparam;
                                    fprintf fmt "store %d, r%d\n" rslot !regparam
                                end);
                            fprintf fmt "load_address r%d, %d\n" !regparam rslot;
                            regparam := !regparam + 1
                        )
                        else (
                            fprintf fmt "load r%d, %d\n" !regparam rslot;
                            (if rsym.sym_typespec = "int" &&
                                lsym.sym_typespec = "float" then
                                fprintf fmt "int_to_real r%d, r%d\n" !regparam !regparam);
                            regparam := !regparam + 1
                        )
                    )
                | LField (id, exprs) -> 
                  let rsym = Symbol.find_symbol id scope in
                  let rslot = rsym.slot_start in
                  let lsym = 
                    (Symbol.find_symbol_by_slot !regparam !regparam procname) in
                    if rsym.pass_by_ref then (
                            ()
                    )
                    else (
                        let reg = !regparam in
                            cg_array fmt (id, exprs) scope reg;
                            fprintf fmt "load_address r%d, %d\n" (reg + 1) rslot;
                            fprintf fmt "sub_offset r%d, r%d, r%d\n" reg (reg + 1) reg;
                            fprintf fmt "load_indirect r%d, r%d\n" (reg+1) reg;
                            if lsym.pass_by_ref then (
                                (if rsym.sym_typespec = "int" &&
                                    lsym.sym_typespec = "float" then
                                    begin
                                        (*fprintf fmt "load r%d, %d\n" !regparam rslot;*)
                                        fprintf fmt "int_to_real r%d, r%d\n" (reg+1) (reg+1);
                                        fprintf fmt "store_indirect r%d, r%d\n" reg (reg+1) 
                                    end);
                                (*fprintf fmt "load r%d, %d\n" reg reg;*)
                                regparam := !regparam + 1
                            )
                            else (
                                (*fprintf fmt "load r%d, %d\n" !regparam rslot;*)
                                (if rsym.sym_typespec = "int" &&
                                    lsym.sym_typespec = "float" then
                                    fprintf fmt "int_to_real r%d, r%d\n" reg (reg+1)
                                else
                                    fprintf fmt "move r%d, r%d\n" reg (reg+1));
                                regparam := !regparam + 1
                            )
                        )
                    )
        | _ -> 
            cg_expr fmt expr scope !regparam;
            let rtype = match_expr scope expr in
                let ltype = (Symbol.find_symbol_by_slot !regparam !regparam procname).sym_typespec in
                    (if ltype = "float" &&
                        rtype = "int" then
                        fprintf fmt "int_to_real r%d, r%d\n" !regparam !regparam); 
                    regparam := !regparam + 1

(* generate code for call statement *)
let cg_call fmt (procname, exprs) scope = 
    fprintf fmt "# call\n";
    regparam := 0;
    List.iter (fun x -> cg_parameter_load fmt procname x scope) exprs;
    fprintf fmt "call proc_%s\n" procname

(* generate code for each statement *)
let rec cg_stmt fmt stmt scope= 
    match stmt with
    | Atomic astmt -> cg_astmt fmt astmt scope
    | Composite cstmt -> cg_cstmt fmt cstmt scope
(*atomic stmt*)
and cg_astmt fmt stmt scope =
    match stmt with
    | Assign (lval, rval) ->
	 fprintf fmt "# assignment\n";
        cg_assign fmt lval rval scope
    | Read lval ->
        cg_read fmt lval scope
    | Write expr ->
        cg_write fmt expr scope
    | ProcCall (id, exprs) ->
        cg_call fmt (id, exprs) scope
(*compund stmt*)
and cg_cstmt fmt stmt scope  =
    match stmt with
    | IfThenFi (expr, stmts)-> 
        cg_if_then fmt (expr, stmts) scope
    | IfThenElseFi (expr, stmt1, stmt2)-> 
        cg_if_then_else fmt (expr, stmt1, stmt2) scope
    | WhileDo (expr, stmts) ->
        cg_while fmt (expr, stmts) scope                   		

(* generate code for ifthen statement *)
and cg_if_then fmt (expr, stmts) scope = 
    labelnum := !labelnum + 1;
    let curlabelnum = !labelnum in
    fprintf fmt "# if\n";
    cg_expr fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label%d\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts;
    fprintf fmt "label%d:\n" curlabelnum
(* generate code for ifthenelse statement *)
and cg_if_then_else fmt (expr, stmts1, stmts2) scope = 
    labelnum := !labelnum + 1;
    let curlabelnum = !labelnum in
    fprintf fmt "# if\n";
    cg_expr fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label%delse\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts1;
    fprintf fmt "branch_uncond label%dafter\n" curlabelnum;
    fprintf fmt "label%delse:\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts2;
    fprintf fmt "label%dafter:\n" curlabelnum
(* generate code for while statement *)
and cg_while fmt (expr, stmts) scope = 
    labelnum := !labelnum + 1;
    let curlabelnum = !labelnum in
    fprintf fmt "# while\n";
    fprintf fmt "label%dcond:\n" curlabelnum;
    cg_expr fmt expr scope 0;
    fprintf fmt "branch_on_false r0, label%dafter\n" curlabelnum;
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts;
    fprintf fmt "branch_uncond label%dcond\n" curlabelnum;
    fprintf fmt "label%dafter:\n" curlabelnum

(* generate code for each variable declaration *)
let cg_vardecl fmt (typespec, ident) scope = 
    match typespec with
        | Bool -> 
            let slotnum = (Symbol.find_symbol ident scope).slot_start in
	    	fprintf fmt "int_const r0, 0\n";
            	fprintf fmt "store %d, r0\n" slotnum
        | Int -> 
            let slotnum = (Symbol.find_symbol ident scope).slot_start in
	    	fprintf fmt "int_const r0, 0\n";	
            	fprintf fmt "store %d, r0\n" slotnum
        | Float ->
	     let slotnum = (Symbol.find_symbol ident scope).slot_start in
	    	fprintf fmt "real_const r0, 0.0\n";	
            	fprintf fmt "store %d, r0\n" slotnum			 
(*array declaration*)
let cg_arrydecl fmt (typespec,id,intervals) scope =
   match typespec with
	| Bool -> 
	  let sym = Symbol.find_symbol id scope in
	  let (head, tail) = (sym.slot_start, sym.slot_end) in	
		fprintf fmt "int_const r0, 0\n";
		for i = head to tail do
			fprintf fmt "store %d, r0\n" i 
		done
 	| Int -> 
	   let sym = Symbol.find_symbol id scope in
	  let (head, tail) = (sym.slot_start, sym.slot_end) in	
		fprintf fmt "int_const r0, 0\n";
		for i = head to tail do
			fprintf fmt "store %d, r0\n" i 
		done
	| Float ->
	  let sym = Symbol.find_symbol id scope in
	  let (head, tail) = (sym.slot_start, sym.slot_end) in	
		fprintf fmt "real_const r0, 0.0\n";
		for i = head to tail do
			fprintf fmt "store %d, r0\n" i 
		done	
(*general declaration*)
let cg_decl fmt decl scope = 
	match decl with
	| DeclVar (typespec, id) ->
		cg_vardecl fmt (typespec,id) scope
	| DecArr (typespec,id,intervals) ->
		cg_arrydecl fmt (typespec,id,intervals) scope

(* generate code for proc body *)
let cg_procbody fmt (decls, stmts) scope = 
    if (List.length decls) > 0 then (
        fprintf fmt "# variable declarations\n";
        (*fprintf fmt "int_const r0, 0\n";*)
        List.iter (fun x -> (cg_decl fmt) x scope) decls
    );
    List.iter (fun x -> (cg_stmt fmt) x scope) stmts

(* generate code for each formal parameter *)
(* store value of formal parameter from register to slot *)
let cg_parameter fmt (Param (passspec,typespec,ident)) scope = 
    match typespec with
        | Bool -> 
            let slotnum = (Symbol.find_symbol ident scope).slot_start in
            fprintf fmt "store %d, r%d\n" slotnum !regparam;
            regparam := !regparam + 1
        | Int -> 
            let slotnum = (Symbol.find_symbol ident scope).slot_start in
            fprintf fmt "store %d, r%d\n" slotnum !regparam;
            regparam := !regparam + 1
        | Float -> 
            let slotnum = (Symbol.find_symbol ident scope).slot_start in
            fprintf fmt "store %d, r%d\n" slotnum !regparam;
            regparam := !regparam + 1

(* generate code for each proc *)
let cg_proc fmt proc = 
    let ((ProcHead (name, params)), (ProcBody (decls, stmts))) = (proc.head, proc.body) in
    let framesize = (Symbol.find_proc name).proc_size in
    fprintf fmt "proc_%s:\n" name;
    fprintf fmt "# prologue\n";
    fprintf fmt "push_stack_frame %d\n" framesize;
    if (List.length params) > 0 then (
        regparam := 0;
        List.iter (
            fun x -> (cg_parameter fmt) x name
        ) params;
    );
    cg_procbody fmt (decls, stmts) name;
    fprintf fmt "# epilogue\n";
    fprintf fmt "pop_stack_frame %d\n" framesize;
    fprintf fmt "return\n"

(* generate code for the whole program *)
let generate_program fmt program =
    fprintf fmt "call proc_main\n";
    fprintf fmt "halt\n";
    fprintf fmt "runerror:\n";
    cg_string fmt "\"divide by 0.\"";
    fprintf fmt "call_builtin print_string\n";
    fprintf fmt "halt\n";
    List.iter (cg_proc fmt) program.procs
