
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
%token STRING            //String type

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token EOL               //Indicates the end of a line
%token SCOLON            //Semicolon, to separate expressions
%token ARROW
%token EOF
%token COMMA

%token LCURLY            //Pairs are created with curly braces
%token RCURLY

%token LIST
%token CONS
%token NIL
%token LBRACKET
%token RBRACKET
%token HEAD
%token TAIL
%token ISNIL

%token <int> INTV
%token <string> STRING_VAL //String value delimited by quotes
%token <string> STRINGV

%start s
%type <Lambda.cmd> s     //Root type is command, terms go inside CmdTerm

//Precedence, to avoid shift/reduce conflicts
%nonassoc SCOLON
%nonassoc BEFORE_SCOLON

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
      { TmIf ($2, $4, $6) }                            %prec BEFORE_SCOLON;
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }                           %prec BEFORE_SCOLON;
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }                         %prec BEFORE_SCOLON;
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) } %prec BEFORE_SCOLON;
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
  | HEAD LBRACKET ty RBRACKET atomicTerm
      { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET atomicTerm
      { TmTail ($3, $5) }
  | ISNIL LBRACKET ty RBRACKET atomicTerm
      { TmIsNil ($3, $5) }
  | atomicTerm DOT INTV
      { TmAccess ($1, $3) }
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
  | STRING_VAL
      { TmStr $1 }
  | tupleTerm
      { TmTuple $1 }
  | recordTerm
      { TmRecord $1 }
  | CONS LBRACKET ty RBRACKET atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
  | NIL LBRACKET ty RBRACKET
      { TmNil $3 }

tupleTerm :
    LPAREN term COMMA RPAREN
      { [$2] }
  | LPAREN term tupleTermR RPAREN
      { $2::(List.rev $3) }

tupleTermR :
    tupleTermR COMMA term
      { $3::$1 }
  | COMMA term
      { [$2] }

recordTerm :
    LCURLY recordTermEntry RCURLY
      { [$2] }
  | LCURLY recordTermEntry recordTermR RCURLY
      { $2::(List.rev $3) }

recordTermR :
    recordTermR COMMA recordTermEntry
      { $3::$1 }
  | COMMA recordTermEntry
      { [$2] }

recordTermEntry :
    STRINGV COLON term
      { ($1, $3) }

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
  | STRING
      { TyStr }
  | tupleTy
      { TyTuple $1 }
  | recordTy
      { TyRecord $1 }
  | LIST LBRACKET ty RBRACKET
      { TyList $3 }

tupleTy :
    LPAREN ty COMMA RPAREN
      { [$2] }
  | LPAREN ty tupleTyR RPAREN
      { $2::(List.rev $3) }

tupleTyR :
    tupleTyR COMMA ty
      { $3::$1 }
  | COMMA ty
      { [$2] }

recordTy :
    LCURLY recordTyEntry RCURLY
      { [$2] }
  | LCURLY recordTyEntry recordTyR RCURLY
      { $2::(List.rev $3) }

recordTyR :
    recordTyR COMMA recordTyEntry
      { $3::$1 }
  | COMMA recordTyEntry
      { [$2] }

recordTyEntry :
    STRINGV COLON ty
      { ($1, $3) }