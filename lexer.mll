
{
  open Parser;;
  exception Lexical_error;; 

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
  | "cons"          { CONS } (* List constructor *)
  | "nil"           { NIL } (* Empty list *)
  | "head"          { HEAD } (* Get list head *)
  | "tail"          { TAIL } (* Get list tail *)
  | "isnil"         { ISNIL } (* Check whether list is empty *)
  | "let"           { LET }
  | "letrec"        { LETREC } (* Used for recursion *)
  | "in"            { IN }
  | "Bool"          { BOOL }
  | "Nat"           { NAT }
  | "Unit"          { UNIT } (* Unit type *)
  | "String"        { STRING } (* String type *)
  | "List"          { LIST } (* List type *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '.'             { DOT }
  | '='             { EQ }
  | ':'             { COLON }
  | "->"            { ARROW }
  | ";;"            { EOL } (* Indicates the end of a line *)
  | ";"             { SCOLON } (* Separates expressions *)
  | ","             { COMMA }
  | "{"             { LCURLY }
  | "}"             { RCURLY }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ['0'-'9']+      { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'[^'"']*'"'   { STRING_VAL (strip_string_quotes (Lexing.lexeme lexbuf)) } (* String value, delimited by quotes *)
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                    { STRINGV (Lexing.lexeme lexbuf) }
  | eof             { EOF }
  | _               { raise Lexical_error } 

