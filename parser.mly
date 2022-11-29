
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token UNIT_VAL          //Unit constant
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token PRINT_NAT
%token PRINT_STRING
%token PRINT_NEWLINE
%token READ_NAT
%token READ_STRING
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token UNIT              //Unit type

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token EOL               //Indicates the end of a line
%token SCOLON            //Semicolon, to separate expressions
%token ARROW
%token EOF

%token <int> INTV
%token <string> STRINGV

%start s
%type <Lambda.cmd> s     //Root type is command, terms go inside CmdTerm

%%

s :
    term EOL EOF
      { CmdTerm ($1) }
  | STRINGV EQ term EOL EOF
      { CmdBind ($1, $3) }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
  | term SCOLON term
      { TmApp (TmAbs (fresh_name "x" (free_vars $3), TyUnit, $3), $1) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | PRINT_NAT atomicTerm
      { TmPrintNat $2 }
  | PRINT_STRING atomicTerm
      { TmPrintString $2 }
  | PRINT_NEWLINE atomicTerm
      { TmPrintNewline $2 }
  | READ_NAT atomicTerm
      { TmReadNat $2 }
  | READ_STRING atomicTerm
      { TmReadString $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | UNIT_VAL
      { TmUnit }
  | STRINGV
      { TmVar $1 }
  | INTV
      { int_to_nat $1 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | UNIT
      { TyUnit }

