(*This file contains function which do pretty printing for 
a snicjk program *)

open Snick_ast
open Format
open Printf

(* this function return the identation  string, i.e whitespace, depend on level*)
let rec indentation = function 
								0 -> ""
								| _  as level -> "    " ^ (indentation (level - 1))


(*this function takes an indicator as input and print out 
the corresponding Snick paramester indicator, rel or val*) 
let print_indicator indicator = match indicator with 		
									| IndiVal -> Format.print_string "val "
									| IndiRef -> Format.print_string "ref "	


(*this function print identifier*)
let print_ident (Ident ident) = Format.print_string ident 


(*this function print basic type*)
let print_beantype beantype = match beantype with
									| Bool  -> Format.print_string "bool "
									| Int   -> Format.print_string "int "
									| Float -> Format.print_string "float "


(*this function print constant*)
let print_const = function 
					| Ebool bconst -> Format.print_bool bconst
					| Eint iconst -> Format.print_int iconst
					| Efloat (fconst, fstring) -> begin
													Format.print_string fstring
												end	
					| EString sconst -> Format.print_string sconst


(*this function print binary operator*)
let print_binop = function 
					  | Op_add  -> Format.print_string " + "
					  | Op_sub  -> Format.print_string " - "
					  | Op_mul  -> Format.print_string " * "
					  | Op_div  -> Format.print_string " / "  
					  | Op_eq   -> Format.print_string " = "
					  | Op_nEq  -> Format.print_string " != "
					  | Op_lt   -> Format.print_string " < "
					  | Op_ltEq -> Format.print_string " <= "
					  | Op_gt   -> Format.print_string " > "
					  | Op_gtEq -> Format.print_string " >= "
					  | Op_or   -> Format.print_string " or "
					  | Op_and	-> Format.print_string " and "


(*print unary operator*)
let print_unop = function
					  | Op_minus -> Format.print_string "-"				
 					  | Op_not -> Format.print_string "not "

(*next three funcs mutually recursive*)

(*print expression according to its type*)
let rec print_expr = 
	function
		| Elval lvalue -> print_lvalue lvalue (*left val*)
		| Const const  -> print_const const (*constant*)
		| ParenExpr expr -> begin (*print (expr)*)
								Format.print_string "(";
								print_expr expr;
								Format.print_string ")"
							end	
		| Ebinop (expr1, binop, expr2) -> begin (*binary op*)
											print_expr expr1;
											print_binop binop;
											print_expr expr2;
										  end				
		| Eunop (unop, expr) -> begin (*unary op*)
									print_unop unop;
									print_expr expr;
								end					
(* print left value *)
and print_lvalue = 
	function 
		| LId ident -> print_ident ident
		| LField (ident, expr_list) -> 
				begin
					print_ident ident;
					Format.print_string "[";
					print_expr_list expr_list;
					Format.print_string "]"; 
			   end 	
(*print list of expression*)
and print_expr_list = 
	function 
		[] -> ()
		| (expr :: []) -> begin (*one expr just print expr*)
							print_expr expr;
							print_expr_list [];
						  end
		| (expr :: expr_list) -> begin (*more than one expr, print expr and ","*)
								print_expr expr;
								Format.print_string ", ";
								print_expr_list expr_list;
							 end

							 						
(*print right value*)
let print_rvalue = function 
						| Rexpr expr -> print_expr expr


(*print assign statement*)
let print_assign astmt = 
	let (lvalue, rvalue) = astmt in
			  			begin
							print_lvalue lvalue;
							Format.print_string " := ";
							print_rvalue rvalue;		  				
			  			end

					  			
(*print read stmt*)
let print_read astmt = begin
						Format.print_string ("read ");
						print_lvalue astmt;
					 end					



(*print write*)
let print_write astmt = begin
							Format.print_string ("write ");
							print_expr astmt;
						  end						


(*print id (expr list)*)				   
let print_proc_call astmt = 
	let (ident, expr_list) = astmt in
							begin
								print_ident ident;
								Format.print_string "(";
								print_expr_list expr_list;
								Format.print_string ")";				   	
							end				   


(*print statement in general, divide
into atomic and composite*)
let rec print_stmt stmt level = 
	match stmt with
		| Atomic astmt ->	begin
								Format.print_string (indentation level);
								print_astmt astmt level;
								Format.print_string ";\n"
							end 
		| Composite cstmt -> begin
								Format.print_string (indentation level);
								print_cstmt cstmt level;
								Format.print_string "\n";
							 end 
										

(*handler to print procedure list of stmts with level is the identation level*)
and print_stmt_list stmts_list level = 
		match stmts_list with
			| [] -> ()
			| stmt :: stmts_list -> 
				begin
					print_stmt stmt level;
					print_stmt_list stmts_list level;
				end

(*print atomic stmt according to its type*)
and print_astmt astmt level = 
	match  astmt with
		| Assign assign     -> print_assign assign 
		| Read read         -> print_read read 
		| Write write       -> print_write write 
		| ProcCall proc_call -> print_proc_call proc_call 


(*print composite stmt according to its type*)
and print_cstmt cstmt level = 
	match cstmt with
		| IfThenFi (expr, stmts_list)                   
			-> print_if expr stmts_list level
		| IfThenElseFi (expr, stmts_list1, stmts_list2) 
			-> print_if_else expr stmts_list1 stmts_list2 level
		| WhileDo (expr, stmts_list)                    
			-> print_while expr stmts_list level

(*print if stmt*)

and print_if expr stmts_list level = 
	begin
		Format.print_string "if ";
		print_expr expr;
		Format.print_string " then\n";
		print_stmt_list stmts_list (level + 1);
		Format.print_string ((indentation level) ^ "fi");
	 end


(*print if else stmt*)
and print_if_else expr stmts_list1 stmts_list2 level = 
	begin
		Format.print_string "if ";
		print_expr expr;
		Format.print_string " then\n";
		print_stmt_list stmts_list1 (level + 1);
		Format.print_string ((indentation level) ^ "else\n");
		print_stmt_list stmts_list2 (level + 1);
		Format.print_string ((indentation level) ^ "fi");
	end


(*print while do stmt*)
and print_while expr stmts_list level = 
	begin
		Format.print_string "while ";
		print_expr expr;
		Format.print_string " do\n";
		print_stmt_list stmts_list (level + 1);
		Format.print_string ((indentation level) ^ "od");
	 end


(*print interval for arry decl*)
let print_interval = fun interval ->
						let (Interval (m, n)) = interval in
							begin
								Format.print_int m;
								Format.print_string "..";
								Format.print_int n;
							end


(*print collection of intervals, ex: [1..2,2..3,4..5]*)

let rec print_intervals = function 
						[] -> Format.print_string "]"
						| (interval :: []) -> 
							begin 
								print_interval interval;
								print_intervals [];
							  end	
						| (interval :: intervals) -> 
								begin
									print_interval interval;
									Format.print_string ",";
									print_intervals intervals;
								 end


(*print arry decl example int a[1..2,2..3]*)
let print_arr_decl = fun decl ->
					let (beantype, ident, intervals) = decl in
						begin
							Format.print_string (indentation 1);
							print_beantype beantype;
							print_ident ident;
							Format.print_string "[";
							print_intervals intervals;
							Format.print_string ";\n";
						end


(*print single variable decl ex float b;*)
let print_var_decl = fun decl ->
					let (beantype, ident) = decl in 
						begin
							Format.print_string (indentation 1);
							print_beantype beantype;
							print_ident ident;
							Format.print_string ";\n";
						end


(*handler to print decl in general can be var or array*)
let print_decl = function
					DeclVar decl -> print_var_decl decl		
					| DecArr decl -> print_arr_decl decl


(*handler to print list of decl in a procedure*)
let rec print_decls = function 
					[] -> Format.print_string "\n"
					|(decl :: decls) -> begin 
											print_decl decl;
											print_decls decls;
										end	


(*handler to print procedure body*)
let print_proc_body = fun body ->
			let (ProcBody (decls, stmts)) = body in 
				begin
					print_decls decls;
					(*Format.print_int (List.length stmts);*)
					print_stmt_list stmts 1;
				end


(*print a paramester decl, ex val int a*)
let print_param = fun param ->
			let (Param (indicator, beantype, ident)) = param in
				begin 
					print_indicator indicator;
					print_beantype beantype;
					print_ident ident;
				end


(*handler to print all paramester decl*)
let rec print_params = function  
					[] -> Format.print_string ")\n"
					| (param :: []) -> 
						begin 
							print_param param;
							print_params [];
						end	
					| (param :: param_list) -> 
						begin
							print_param param;
							Format.print_string ", ";
							print_params param_list;
						end

(*handler to print procedure head*)

let print_proc_head = fun head  ->
						let (ProcHead (Ident ident, param_list)) = head in 
							begin
								Format.print_string (ident ^ " (");
								print_params param_list; 
							end


(*handler to print a procedure*)
let print_proc = fun proc -> 
							begin
								Format.print_string "proc ";
								print_proc_head proc.head;
								print_proc_body proc.body;
								Format.print_string "end\n\n";	
							end


(*handler to print all procedures*)
let print_procs = fun procList -> 
							List.iter print_proc procList 


(*handler to print a program storeed in prog using snick_ast.program type*)
let print_program fmt prog = print_procs prog.procs 





































