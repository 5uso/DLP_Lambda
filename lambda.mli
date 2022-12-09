
type ty =
  TyBool
| TyNat
| TyArr of ty * ty
| TyUnit (* Unit type *)
| TyStr (* String type *)
| TyPair of ty * ty (* Pair type *)
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
| TmPair of term * term (* Pair term *)
| TmAccess of term * int (* Nth component of a tuple *)
| TmNil of ty
| TmCons of ty * term * term
| TmIsNil of ty * term
| TmHead of ty * term
| TmTail of ty * term
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

val emptyctx : context;;
val addbinding : context -> string -> ty -> term -> context;;
val getbinding : context -> string -> (string * ty * term);;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
val free_vars : term -> string list;;
val fresh_name : string -> string list -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;

val substall : context -> term -> term;;
(* Executes a command, returning the updated context *)
val run_cmd : context -> cmd -> context;;
val check_cmd : context -> cmd -> context;;
val run_cmd_silent : context -> cmd -> context;;