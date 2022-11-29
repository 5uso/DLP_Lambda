
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t' '\n']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "()"        { UNIT_VAL } (* Unit value *)
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC } (* Used for recursion *)
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "Unit"      { UNIT } (* Unit type *)
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ";;"        { EOL } (* Indicates the end of a line *)
  | ";"         { SCOLON } (* Separates expressions *)
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

