(*this file define the lexer generator for ocamllex*)
(* Author: Team Name - O'Harry the Caml: 
 *            - Wilkins J Leong
 *            - Tuyen Quang Pham
 *            - Jorge Ricardo Pardave Garcia *)                                        
{
open Snick_parse
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
let stringconst = '\"'[^ '"' '\t' '\n']*'\"'
let comment = '#'[^ '\n']* 
rule token = parse (*lexing rule for each type of token*)
  [' ' '\t']   { token lexbuf }     (* skip blanks *)
  |  '\n'          { Lexing.new_line lexbuf ; token lexbuf } 
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) } 
  | '-'?['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT_CONST(lxm) } 
  | stringconst as lxm { STRING_CONST( lxm)}
  (* keywords *)
  | "bool" { BOOL }
  | "int" { INT }
  | "float" { FLOAT }
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ }
  | "write" { WRITE }
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '=' { EQ }
  | '<' { LT }
  | '+' { PLUS }
  | '-' { UMINUS }
  | " - " { MINUS } 
  | '*' { MUL }
  | ';' { SEMICOLON }
  | '/' { DIVIDE }
  | "!=" { NONEQ }
  | "<=" { LTEQ }
  | '>' { GT }
  | ">=" { GTEQ }
  | "or" { OR } 
  | "and" { AND } 
  | "not" { NOT }   
  | "end" { END }
  | "proc" { PROC }
  | ',' { COMMA }
  | ".." { DOTDOT }
  | '[' { LBRAC }
  | ']' { RBRAC }
  | "while" { WHILE }
  | "do" { DO }
  | "od" { OD }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | "val" { VAL }
  | "ref" { REF }
  | ident as lxm { IDENT lxm }
  | eof { EOF }
  | '#' { line_comment "" lexbuf}
and line_comment buff = parse (* skip comments*)
    '\n' { Lexing.new_line lexbuf ; token lexbuf }
    | _ {line_comment "" lexbuf}
