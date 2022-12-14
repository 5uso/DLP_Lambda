(*
  Lambda Interface,
  by Jesús Mosteiro García and Luca D’angelo Sabin
    (jesus.mosteiro@udc.es)    (l.dangelo@udc.es)

  Made for the programming language design course at UDC.
*)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyUnit                         (* Unit type *)
  | TyStr                          (* String type *)
  | TyTuple of ty list             (* Tuple type, with a list of its element's types *)
  | TyRecord of (string * ty) list (* Record type, with a list of its element's names and types *)
  | TyList of ty                   (* List type, with its element type *)
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
  | TmFix of term                    (* Fix term, used for recursion *)
  | TmStr of string                  (* String term, uses ocaml strings *)
  | TmUnit                           (* Unit term *)
  | TmTuple of term list             (* Tuple term, with a list of its elements *)
  | TmRecord of (string * term) list (* Record term, with a list of its named elements *)
  | TmAccess of term * int           (* Projection, gets Nth component of a tuple *)
  | TmAccessNamed of term * string   (* Projection, gets named component of a record *)
  | TmNil of ty                      (* Empty list term, explicitly typed *)
  | TmCons of ty * term * term       (* List cons term, explicitly typed *)
  | TmIsNil of ty * term             (* Is list empty? Explicitly typed *)
  | TmHead of ty * term              (* Head of list, explicitly typed *)
  | TmTail of ty * term              (* Tail of list, explicitly typed *)
  | TmConcat of term * term          (* Concatenation term for strings *)
;;

(* Context now keeps track of values as well as types *)
type context =
  (string * ty * term) list
;;

(* Support for statements that aren't treated as terms *)
type cmd = 
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