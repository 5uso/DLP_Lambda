
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t' '\r' '\n']  { token lexbuf }
  | "lambda"        { LAMBDA }
  | "L"             { LAMBDA }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "()"            { UNIT_VAL } (* Unit value *)
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "succ"          { SUCC }
  | "pred"          { PRED }
  | "iszero"        { ISZERO }
  | "print_nat"     { PRINT_NAT }
  | "print_string"  { PRINT_STRING }
  | "print_newline" { PRINT_NEWLINE }
  | "read_nat"      { READ_NAT }
  | "read_string"   { READ_STRING }
  | "let"           { LET }
  | "letrec"        { LETREC } (* Used for recursion *)
  | "in"            { IN }
  | "Bool"          { BOOL }
  | "Nat"           { NAT }
  | "Unit"          { UNIT } (* Unit type *)
  | "String"        { STRING } (* String type *)
  | "List"          { LIST } (* List type *)
  | "head"          { HEAD } (* List head *)
  | "tail"          { TAIL } (* List tail *)
  | "isempty"       { ISEMPTY } (* List is empty *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '.'             { DOT }
  | '='             { EQ }
  | ':'             { COLON }
  | "->"            { ARROW }
  | ";;"            { EOL } (* Indicates the end of a line *)
  | ";"             { SCOLON } (* Separates expressions *)
  | "\""            { QUOTE } (* Indicates the limits of a string *)
  | ","             { COMMA }
  | "{"             { LCURLY }
  | "}"             { RCURLY }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ".1"            { FIRST }
  | ".2"            { SECOND }
  | ['0'-'9']+      { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                    { STRINGV (Lexing.lexeme lexbuf) }
  | eof             { EOF }
  | _               { raise Lexical_error } 

