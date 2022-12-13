
{
  open Parser;;

  (* Added a string to describe lexical errors *)
  exception Lexical_error of string;; 

  (* Strips the quotes off a string, used for string literals *)
  let strip_string_quotes str =
    match String.length str with
      0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)
}

rule token = parse
    [' ' '\t' '\r' '\n']  { token lexbuf }
  | "lambda"        { LAMBDA }
  | "L"             { LAMBDA }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "()"            { UNIT_VAL }                                                (* Unit value *)
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "succ"          { SUCC }
  | "pred"          { PRED }
  | "iszero"        { ISZERO }
  | "print_nat"     { PRINT_NAT }                                               (* Print a nat given its term *)
  | "print_string"  { PRINT_STRING }                                            (* Print a string given its term *)
  | "print_newline" { PRINT_NEWLINE }                                           (* Print a newline character *)
  | "read_nat"      { READ_NAT }                                                (* Read a nat from stdin *)
  | "read_string"   { READ_STRING }                                             (* Read a string from stdin *)
  | "cons"          { CONS }                                                    (* List constructor *)
  | "nil"           { NIL }                                                     (* Empty list *)
  | "head"          { HEAD }                                                    (* Get list head *)
  | "tail"          { TAIL }                                                    (* Get list tail *)
  | "isnil"         { ISNIL }                                                   (* Checks whether a list is empty *)
  | "let"           { LET }
  | "letrec"        { LETREC }                                                  (* Used for recursion, translates into fix *)
  | "in"            { IN }
  | "Bool"          { BOOL }
  | "Nat"           { NAT }
  | "Unit"          { UNIT }                                                    (* Unit type *)
  | "String"        { STRING }                                                  (* String type *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '.'             { DOT }
  | '='             { EQ }
  | ':'             { COLON }
  | "->"            { ARROW }
  | ";;"            { EOL }                                                     (* Two semicolons indicate the end of a line *)
  | ";"             { SCOLON }                                                  (* Semicolons separate expressions *)
  | ","             { COMMA }                                                   (* Separates collection elements *)
  | "{"             { LCURLY }                                                  (* Curly brackets enclose records *)
  | "}"             { RCURLY }
  | "["             { LBRACKET }                                                (* Square brackets enclose lists *)
  | "]"             { RBRACKET }
  | "^"             { CONCAT }                                                  (* String concatenation symbol *)
  | ['0'-'9']+      { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'[^'"']*'"'   { STRING_VAL (strip_string_quotes (Lexing.lexeme lexbuf)) } (* String value, delimited by quotes *)
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                    { STRINGV (Lexing.lexeme lexbuf) }
  | eof             { EOF }
                    
                    (* Lexical errors point out the unexpected string *)
  | _               { raise (Lexical_error ("unexpected '" ^ (Lexing.lexeme lexbuf) ^ "'")) } 
