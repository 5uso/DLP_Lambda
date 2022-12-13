
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
%token PRINT_NAT         //print_nat kernel function
%token PRINT_STRING      //print_string kernel function
%token PRINT_NEWLINE     //print_newline kernel function
%token READ_NAT          //read_nat kernel function
%token READ_STRING       //read_String kernel function
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token UNIT              //Unit type

%token STRING            //String type
%token CONCAT            //String concatenation operator

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token EOL               //Indicates the end of a line
%token SCOLON            //Semicolon, to separate expressions
%token ARROW
%token EOF
%token COMMA             //Comma, to separate collection elements

%token LCURLY            //Records are created with curly braces
%token RCURLY

%token CONS              //List constructor
%token NIL               //Nil represents an empty list
%token LBRACKET          //Square brackets enclose lists
%token RBRACKET
%token HEAD              //head kernel function
%token TAIL              //tail kernel function
%token ISNIL             //isnil kernel function

%token <int> INTV
%token <string> STRING_VAL //String literal delimited by quotes
%token <string> STRINGV

%start s
%type <Lambda.cmd> s     //Root type is command, terms go inside CmdTerm

//Precedence, to avoid shift/reduce conflicts
%nonassoc SCOLON
%nonassoc BEFORE_SCOLON

%%

s :
    //Term statements have to end in ;;
    term EOL EOF
      { CmdTerm ($1) }
    //Bind statements run an alternative command that affects the global context
  | STRINGV EQ term EOL EOF
      { CmdBind ($1, $3) }

//Term precedence had to be tweaked to avoid conflicts with semicolons, which should have the least precedence
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }                            %prec BEFORE_SCOLON;
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }                           %prec BEFORE_SCOLON;
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }                         %prec BEFORE_SCOLON;
    //Letrec gets translated into fix application in the parser
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) } %prec BEFORE_SCOLON;
    //Semicolons separate terms by translating them into unit lambda applications
  | term SCOLON term
      { TmApp (TmAbs (fresh_name "x" (free_vars [] $3), TyUnit, $3), $1) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
    //Kernel functions are considered application terms
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
    //Appterms related to lists are explicitly typed
  | HEAD LBRACKET ty RBRACKET atomicTerm
      { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET atomicTerm
      { TmTail ($3, $5) }
  | ISNIL LBRACKET ty RBRACKET atomicTerm
      { TmIsNil ($3, $5) }
  | CONS LBRACKET ty RBRACKET atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
    //String concatenation is a binary operator
  | atomicTerm CONCAT atomicTerm
      { TmConcat ($1, $3) }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
    //Units are atomic
  | UNIT_VAL
      { TmUnit }
  | STRINGV
      { TmVar $1 }
  | INTV
      { int_to_nat $1 }
    //Strings are atomic
  | STRING_VAL
      { TmStr $1 }
    //Tuples are considered atomic because they are already surrounded by ()
  | tupleTerm
      { TmTuple $1 }
    //Records are considered atomic because they are already surrounded by {}
  | recordTerm
      { TmRecord $1 }
    //Nil is considered atomic
  | NIL LBRACKET ty RBRACKET
      { TmNil $3 }
    //The alternative list constructor "Type[element1,element2...]" is considered atomic 
  | listAlt
      { list_to_cons $1 }
    //Projection is considered atomic for convenience, as it doesn't cause ambiguity
  | atomicTerm DOT INTV
      { TmAccess ($1, $3) }
  | atomicTerm DOT STRINGV
      { TmAccessNamed ($1, $3) }

tupleTerm :
    //Tuples of 1 element are in the form "(x,)", same as in python
    LPAREN term COMMA RPAREN
      { [$2] }
    //Tuple with multiple terms
  | LPAREN term tupleTermR RPAREN
      { $2::(List.rev $3) }

tupleTermR :
    //n-tuples
    tupleTermR COMMA term
      { $3::$1 }
  | COMMA term
      { [$2] }

recordTerm :
    //Single entry records
    LCURLY recordTermEntry RCURLY
      { [$2] }
    //Records with multiple entries
  | LCURLY recordTermEntry recordTermR RCURLY
      { $2::$3 }

recordTermR :
    //n-entry record
    recordTermR COMMA recordTermEntry
      { $3::$1 }
  | COMMA recordTermEntry
      { [$2] }

//A single record entry, in the form "name:value"
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
    //Tuple type is considered atomic because it's surrounded by ()
  | tupleTy
      { TyTuple $1 }
    //Record type is considered atomic because it's surrounded by {}
  | recordTy
      { TyRecord $1 }
    //List types are represented as "[element_type]", which is atomic
  | LBRACKET ty RBRACKET
      { TyList $2 }

//Tuple types are a list of the types for each tuple term
tupleTy :
    //Tuples of 1 element are in the form "(x,)", same as in python
    LPAREN ty COMMA RPAREN
      { [$2] }
  | LPAREN ty tupleTyR RPAREN
      { $2::(List.rev $3) }

tupleTyR :
    tupleTyR COMMA ty
      { $3::$1 }
  | COMMA ty
      { [$2] }

//Record types are a list of entries, each with a name and a type
recordTy :
    LCURLY recordTyEntry RCURLY
      { [$2] }
  | LCURLY recordTyEntry recordTyR RCURLY 
      { $2::$3 }

recordTyR :
    recordTyR COMMA recordTyEntry
      { $3::$1 }
  | COMMA recordTyEntry
      { [$2] }

//Record type entries are similar to the syntax of the record term
recordTyEntry :
    STRINGV COLON ty
      { ($1, $3) }

//Alternative constructor for lists
listAlt :
    ty LBRACKET listAltR RBRACKET
      { ($1, $3) }

listAltR :
    term
      { [$1] }
  | term COMMA listAltR
      { $1::$3 }
  | /* Empty */
      { [] }