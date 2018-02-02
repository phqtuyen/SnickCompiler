(* Specification of an AST for snick program
*)

exception Except of string
(*identifier name in snick *)
type ident = string

(* basic type in snick. *)
type beantype =
  | Bool
  | Int
  | Float

(*constant value in snick program*)
type const =
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | EString of string

(*binary operation in snick*)
type binop = (* must be infix or produce error*)
  | Op_add  
  | Op_sub 
  | Op_mul 
  | Op_div  
  | Op_eq 
  | Op_nEq
  | Op_lt 
  | Op_ltEq
  | Op_gt
  | Op_gtEq
  | Op_or 
  | Op_and

type unop = (*unary operator*)
  | Op_minus
  | Op_not

(*basic expression
left val or const or (expr) or binary op
unary op*)
type expr =
  | Elval of lvalue
  | Const of const
  | ParenExpr of expr (* (expr) if miss one bracket then produce error*)
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
and lvalue =
  | LId of ident
  | LField of (ident * (expr list)) (*id <expr list>*)  

(* right value  *)
type rvalue =
  | Rexpr of expr

(*snick interval m..n , m and n are int*)
type interval = Interval of (int * int)

(*list of intervals 1..2,3..4,5..6 etc*)
type intervals = interval list

(*a declaration  var or array decl*)
type decl = 
  | DeclVar of (beantype * ident) (*int a*)
  | DecArr of (beantype * ident * intervals) (*int a[1..2,3..2]*)

(* snick statement atomic or composite*)
type stmt =
  | Atomic of astmt
  | Composite of cstmt
and astmt = 
  | Assign of (lvalue * rvalue) 
  | Read of lvalue
  | Write of expr 
  | ProcCall of (ident * (expr list)) (* id (expr list)*)
and cstmt = 
  | IfThenFi of (expr * (stmt list)) (*if ... then ... fi*)
  | IfThenElseFi of (expr * (stmt list) * (stmt list)) (*if ... then ... else ... fi*)
  | WhileDo of (expr * (stmt list)) (*while ... do ... od*)

(*parameter passing indicator val or ref*)
type indicator = 
  | IndiVal
  | IndiRef

(*paramester information*)
type parameter = Param of (indicator * beantype * ident)

(* procedure head contains its name and param list*)
type procedureHead = ProcHead of (ident * (parameter list))

(* procedure body decl and stmts*)
type procedureBody = ProcBody of ((decl list) * (stmt list))

(*procedure its head and body*)
type proc = {
  head : procedureHead ;
  body : procedureBody
  (*comments : comment list;*)
}
 
 (*program is a list of procedure*)
type program = {
  procs : proc list;
  (*comments : comment list; *)
}

type t = program
