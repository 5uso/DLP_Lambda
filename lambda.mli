
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term (* Used for recursion *)
;;

(* Context now keeps track of values as well as types *)
type context =
  (string * ty * term) list
;;

type cmd = (* For statements that aren't treated as terms *)
    CmdTerm of term
  | CmdBind of string * term
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> term -> context;;
val getbinding : context -> string -> (string * ty * term);;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;

val substall : context -> term -> term;;
(* Executes a command, returning the updated context *)
val run_cmd : context -> cmd -> context;;