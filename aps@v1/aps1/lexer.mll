{
   open Parser
   exception Eof
}

let integer = '-'?['0'-'9']+
let ident   = (['a'-'z''A'-'Z'])(['a'-'z''A'-'Z''0'-'9'])*
let spaces  = [' ' '\t' '\n']+


rule token = parse
     spaces {token lexbuf}
     | integer as x {INT(int_of_string x)}
     | "[" {LBRACKET}
     | "]" {RBRACKET}
     | "(" {LPAR}
     | ")" {RPAR}
     | ";" {SEMICOLON}
     | "," {COMMA}
     | "*" {STAR}
     | "->" {ARROW}
     | ":" {COLON}
     | "CONST" {CONST}
     | "FUN" {FUN}
     | "REC" {REC}
     | "ECHO" {ECHO}
     | "VAR" {VAR}
     | "PROC" {PROC}
     | "SET" {SET}
     | "IF" {IFStat}
     | "WHILE" {WHILE}
     | "CALL" {CALL}
     | "if" {IF}
     | "bool" {TBOOL}
     | "int" {TINT}
     | "true" as t {TRUE(bool_of_string t)}
     | "false" as f {FALSE(bool_of_string f)}
     | "not" {NOT}
     | "and" {AND}
     | "or" {OR}
     | "eq" {EQ}
     | "lt" {LT}
     | "add" {ADD}
     | "sub" {SUB}
     | "mul" {MUL}
     | "div" {DIV}
     | ident as id {ID(id)}    
     | eof {EOF}