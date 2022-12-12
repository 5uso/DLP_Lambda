
type ty =
  TyBool
| TyNat
| TyArr of ty * ty
| TyUnit (* Unit type *)
| TyStr (* String type *)
| TyTuple of ty list (* Tuple type *)
| TyRecord of (string * ty) list (* Record type *)
| TyList of ty (* List type *)
;;

type term =
  TmTrue
| TmFalse
| TmIf of term * term * term
| TmZero
| TmSucc of term
| TmPred of term
| TmIsZero of term
| TmPrintNat of term
| TmPrintString of term
| TmPrintNewline of term
| TmReadNat of term
| TmReadString of term
| TmVar of string
| TmAbs of string * ty * term
| TmApp of term * term
| TmLetIn of string * term * term
| TmFix of term (* Used for recursion *)
| TmStr of string (* String term *)
| TmUnit (* Unit term *)
| TmTuple of term list (* Tuple term *)
| TmRecord of (string * term) list (* Record term *)
| TmAccess of term * int (* Nth component of a tuple *)
| TmAccessNamed of term * string (* Named component of a record *)
| TmNil of ty (* Nil constructor for lists, indicates the last component *)
| TmCons of ty * term * term (* Constructor for lists *)
| TmIsNil of ty * term (* Check if list component is nil *)
| TmHead of ty * term (* Term to get the head of a list *)
| TmTail of ty * term (* Term to get the tail of a list *)
| TmConcat of term * term (* Concatenation term for strings *)
;;

(* Context now keeps track of values as well as types *)
type context =
  (string * ty * term) list
;;

type cmd = (* For statements that aren't treated as terms *)
    CmdTerm of term
  | CmdBind of string * term
;;

(* Moved int to nat conversion here for convenience *)
val int_to_nat : int -> term;;
val list_to_cons: ty * term list -> term;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> term -> context;;
val getbinding : context -> string -> (string * ty * term);;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
val free_vars : context -> term -> string list;;
val fresh_name : string -> string list -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;

val substall : context -> term -> term;;
(* Executes a command, returning the updated context *)
val run_cmd : context -> cmd -> context;;
val check_cmd : context -> cmd -> context;;
val run_cmd_silent : context -> cmd -> context;;