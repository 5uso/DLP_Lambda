
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyUnit (* Unit type *)
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
  | TmUnit (* Unit term *)
;;

(* Context now keeps track of values as well as types *)
type context =
  (string * ty * term) list
;;

type cmd = (* For statements that aren't treated as terms *)
    CmdTerm of term
  | CmdBind of string * term
;;


(* Int to Nat *)
let rec int_to_nat = function
    0 -> TmZero
  | n -> TmSucc (int_to_nat (n - 1))
;;


(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x ty value =
  (x, ty, value) :: ctx
;;

let getbinding ctx x =
  let f a b = match b with (c, _, _) -> c = a
  in List.find (f x) ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyUnit ->
      "Unit"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-Unit *)
  | TmUnit ->
      TyUnit

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-PrintNat *)
  | TmPrintNat t1 ->
      if typeof ctx t1 = TyNat then TyUnit
      else raise (Type_error "argument of print_nat is not a number")

    (* T-PrintString *)
  | TmPrintString t1 ->
      if typeof ctx t1 = TyUnit then TyUnit (* TODO *)
      else raise (Type_error "argument of print_string is not a string")

    (* T-PrintNewline *)
  | TmPrintNewline t1 ->
      if typeof ctx t1 = TyUnit then TyUnit
      else raise (Type_error "argument of print_newline is not unit")

    (* T-ReadNat *)
  | TmReadNat t1 ->
      if typeof ctx t1 = TyUnit then TyNat
      else raise (Type_error "argument of read_nat is not unit")

    (* T-ReadString *)
  | TmReadString t1 ->
      if typeof ctx t1 = TyUnit then TyUnit (* TODO *)
      else raise (Type_error "argument of read_string is not unit")

    (* T-Var *)
  | TmVar x ->
      (try (match getbinding ctx x with (_, ty, _) -> ty) with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 t2 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
            TyArr (tyT11, tyT12) ->
              if tyT2 = tyT11 || tyT11 = TyUnit then tyT12 (* For simplicity, any type can be "casted" to unit *)
              else raise (Type_error "parameter type mismatch")
          | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 t1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix (t1) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmUnit ->
      "()"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmPrintNat t ->
      "print_nat " ^ "(" ^ string_of_term t ^ ")"
  | TmPrintString t ->
      "print_string " ^ "(" ^ string_of_term t ^ ")"
  | TmPrintNewline t ->
      "print_newline " ^ "(" ^ string_of_term t ^ ")"
  | TmReadNat t ->
      "read_nat " ^ "(" ^ string_of_term t ^ ")"
  | TmReadString t ->
      "read_string " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix (t1) ->
      "(fix " ^ string_of_term t1 ^ ")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

(* TODO: this may need updating to be compatible with global context *)
let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmUnit ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmPrintNat t ->
      free_vars t
  | TmPrintString t ->
      free_vars t
  | TmPrintNewline t ->
      free_vars t
  | TmReadNat t ->
      free_vars t
  | TmReadString t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix (t1) ->
      free_vars t1
;;

(* TODO: this may need updating to be compatible with global context *)
let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmUnit ->
      TmUnit
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmPrintNat t ->
      TmPrintNat (subst x s t)
  | TmPrintString t ->
      TmPrintString (subst x s t)
  | TmPrintNewline t ->
      TmPrintNewline (subst x s t)
  | TmReadNat t ->
      TmReadNat (subst x s t)
  | TmReadString t ->
      TmReadString (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
          if not (List.mem y fvs)
          then TmAbs (y, tyY, subst x s t)
          else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
          if not (List.mem y fvs)
          then TmLetIn (y, subst x s t1, subst x s t2)
          else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix (t1) ->
      TmFix (subst x s t1)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmUnit  -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

(* Evaluation now requires context to use global definitions *)
let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-PrintNat *)
  | TmPrintNat t1 ->
      print_string (string_of_term t1);
      TmUnit

    (* E-PrintString *)
  | TmPrintString t1 ->
    print_string (string_of_term t1);
    TmUnit

    (* E-PrintNewline *)
  | TmPrintNewline t1 ->
    print_string "\n";
    TmUnit

    (* E-ReadNat *)
  | TmReadNat t1 ->
    int_to_nat (read_int ()) (* TODO: Should this return 0 with invalid input? Currently crashes *)

    (* E-ReadString *)
  | TmReadString t1 ->
    TmUnit (* TODO *)

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix (t1) ->
      let t1' = eval1 ctx t1 in
      TmFix(t1')

    (* Variables *)
  | TmVar (y) ->
      (try (match getbinding ctx y with (_, _, value) -> value) with
      _ -> raise (Type_error ("no binding value for variable " ^ y)))

  | _ ->
      raise NoRuleApplies
;;

(* Evaluation now requires context to use global definitions *)
let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> tm
;;


let rec substall ctx tm =
  match ctx with (x, _, s) :: tl ->
    subst x s tm
  | _ -> tm
;;

(* Executes a command, returning the updated context *)
let run_cmd ctx = function
    CmdTerm (tm) -> 
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval ctx tm) ^ " : " ^ string_of_ty tyTm);
      ctx
  | CmdBind (x, bind) ->
      let bind = eval ctx bind in (* Evaluate whatever we can *)
      let bind = substall ctx bind in (* Replace with current context to ensure "functional-like" globals *)
      addbinding ctx x (typeof ctx bind) bind
;;