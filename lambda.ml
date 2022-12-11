
(* TYPE DEFINITIONS *)

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
  | TmNil of ty (* Empty list term *)
  | TmCons of ty * term * term (* List cons term *)
  | TmIsNil of ty * term (* Is list empty *)
  | TmHead of ty * term (* Head of list *)
  | TmTail of ty * term (* Tail of list *)
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

(* List constructor syntax sugar *)
let rec list_to_cons = function
    (ty, l) -> (
      let rec internal l =
        match l with
            hd :: tl -> TmCons (ty, hd, internal tl)
          | [] -> TmNil ty
      in internal l
    )
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
  | TyUnit ->
      "Unit"
  | TyArr (ty1, ty2) ->
      (match ty1 with
          TyArr (_, _) -> "(" ^ string_of_ty ty1 ^ ")"
        | _ -> string_of_ty ty1
      ) ^ " -> " ^
      (match ty2 with
          TyArr (_, _) -> "(" ^ string_of_ty ty2 ^ ")"
        | _ -> string_of_ty ty2
      )
  | TyStr ->
      "String"
  | TyTuple types ->
      let len = List.length types in
      let rec tuple_str t acc =
        match t with 
            hd :: [] -> acc ^ (string_of_ty hd) ^ (if len > 1 then ")" else ",)")
          | hd :: tl -> tuple_str tl (acc ^ (string_of_ty hd) ^ ", ")
          | [] -> acc ^ (if len > 1 then ")" else ",)")
      in tuple_str types "("
  | TyRecord entries ->
      let rec record_str t acc =
        match t with
            (name, rty) :: [] -> acc ^ name ^ ": " ^ (string_of_ty rty) ^ "}"
          | (name, rty) :: tl -> record_str tl (acc ^ name ^ ": " ^ (string_of_ty rty) ^ ", ")
          | [] -> acc ^ "}"
      in record_str entries "{"
  | TyList t -> 
      "[" ^ string_of_ty t ^ "]"
;;

let rec is_subtype super ty = match super with
    TyBool ->
      ty = TyBool
  | TyNat ->
      ty = TyNat
  | TyStr ->
      ty = TyStr
  | TyUnit ->
      true (* TODO: Accept anything as unit, is this ok? *)
  | TyArr (super1, super2) ->
      (match ty with
          TyArr (ty1, ty2) -> (is_subtype super1 ty1) && (is_subtype super2 ty2)
        | _ -> false)
  | TyTuple types -> (* Tuples can be subtyped by smaller tuples that have the same starting types *)
      (match ty with
          TyTuple subtypes -> (
            let rec tuple_sub sup sub =
              match sup with
                  [] -> (match sub with [] -> true | _ -> false)
                | shd :: stl -> (match sub with hd :: tl when hd = shd -> true | _ -> false)
            in tuple_sub types subtypes
          )
        | _ -> false) 
  | TyRecord entries -> (* Records can be subtyped if all named entries of the super appear in the sub with the same type *)
      (match ty with
          TyRecord subentries ->
            List.fold_left (&&) true (List.rev_map (function x -> List.exists ((=) x) subentries) entries)
        | _ -> false)
  | TyList super1 ->
      (match ty with
          TyList ty1 -> is_subtype super1 ty1
        | _ -> false)
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
      if is_subtype TyBool (typeof ctx t1) then
        let tyT2 = typeof ctx t2 in
        if is_subtype tyT2 (typeof ctx t3) then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if is_subtype TyNat (typeof ctx t1) then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if is_subtype TyNat (typeof ctx t1) then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if is_subtype TyNat (typeof ctx t1) then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-PrintNat *)
  | TmPrintNat t1 ->
      if is_subtype TyNat (typeof ctx t1) then TyUnit
      else raise (Type_error "argument of print_nat is not a number")

    (* T-PrintString *)
  | TmPrintString t1 ->
      if is_subtype TyStr (typeof ctx t1) then TyUnit
      else raise (Type_error "argument of print_string is not a string")

    (* T-PrintNewline *)
  | TmPrintNewline t1 ->
      if is_subtype TyUnit (typeof ctx t1) then TyUnit
      else raise (Type_error "argument of print_newline is not unit")

    (* T-ReadNat *)
  | TmReadNat t1 ->
      if is_subtype TyUnit (typeof ctx t1) then TyNat
      else raise (Type_error "argument of read_nat is not unit")

    (* T-ReadString *)
  | TmReadString t1 ->
      if is_subtype TyUnit (typeof ctx t1) then TyStr
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
              if is_subtype tyT11 tyT2 then tyT12
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
            if is_subtype tyT12 tyT11 then tyT12 (* TODO: Check that this works *)
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))
  
    (* T-Str *)
  | TmStr _ ->
      TyStr
  
    (* T-Tuple *)
  | TmTuple terms ->
      let rec tuple_type t acc =
        match t with
            hd :: tl -> tuple_type tl ((typeof ctx hd)::acc)
          | [] -> acc
      in TyTuple (List.rev (tuple_type terms []))

    (* T-Record *)
  | TmRecord entries ->
      let rec record_type t acc =
        match t with
            (name, rt) :: tl -> record_type tl ((name, (typeof ctx rt))::acc)
          | [] -> acc
      in TyRecord (record_type entries [])

    (* T-Access *)
  | TmAccess (t, n) ->
      let t' = typeof ctx t in
      (match t' with
          TyTuple types -> (
            let rec access_type t i =
              match t with
                  hd :: tl -> if i = n then hd
                  else access_type tl (i + 1)
                | [] -> raise (Type_error "Tuple projection out of bounds")
            in access_type types 1
          )
        | _ -> raise (Type_error "Can only apply numbered projection to a tuple"))

    (* T-AccessNamed *)
  | TmAccessNamed (t, n) ->
      let t' = typeof ctx t in
      (match t' with
          TyRecord entries -> (
            let rec access_named_type t =
              match t with
                  (name, rty) :: tl -> if name = n then rty else access_named_type tl
                | [] -> raise (Type_error "Record projection didn't match")
            in access_named_type entries
          )
        | _ -> raise (Type_error "Can only apply named projection to a record"))

    (* T-Cons *)
  | TmCons (ty, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match (ty, tyT1, tyT2) with
          (tyT1', tyT2', TyList tyT3') when tyT1' = tyT2' && tyT1' = tyT3' -> 
            TyList tyT1'
        | (_, _, _) -> raise (Type_error "Type mismatch in cons"))

    (* T-Nil *)
  | TmNil t ->
      TyList t

    (* T-IsNil *)
  | TmIsNil (ty, t) ->
      let tyT = typeof ctx t in
      (match tyT with
          TyList lty when lty = ty -> TyBool
        | _ -> raise (Type_error ("Argument of is_nil[" ^ string_of_ty ty ^ "] must be a list[" ^ string_of_ty ty ^ "]")))
      
    (* T-Head *)
  | TmHead (ty, t) ->
      let tyT = typeof ctx t in
      (match tyT with
          TyList lty when lty = ty -> ty
        | _ -> raise (Type_error ("Argument of head[" ^ string_of_ty ty ^ "] must be a list[" ^ string_of_ty ty ^ "]")))
    
    (* T-Tail *)
  | TmTail (ty, t) ->
      let tyT = typeof ctx t in
      (match tyT with
          TyList lty when lty = ty -> TyList ty
        | _ -> raise (Type_error ("Argument of tail[" ^ string_of_ty ty ^ "] must be a list[" ^ string_of_ty ty ^ "]")))
;;
      

(* TERMS MANAGEMENT (EVALUATION) *)

let term_precedence = function
    TmUnit
  | TmTrue
  | TmFalse
  | TmZero
  | TmVar _
  | TmStr _
  | TmNil _ -> 0
  | TmTuple _
  | TmRecord _ -> 1
  | TmSucc t ->
      let rec f n t' = match t' with
          TmZero -> 0
        | TmSucc s -> f (n + 1) s
        | _ -> 2
      in f 1 t
  | TmPred _
  | TmIsZero _
  | TmPrintNat _
  | TmPrintString _
  | TmPrintNewline _
  | TmReadNat _
  | TmAccess (_, _)
  | TmAccessNamed (_, _)
  | TmHead (_, _)
  | TmTail (_, _)
  | TmIsNil (_, _)
  | TmReadString _ -> 2
  | TmCons (_, _, _) -> 2
  | TmIf (_, _, _) -> 3
  | TmAbs (_, _, _) -> 4
  | TmFix _ -> 5
  | TmApp (_, _) -> 2
  | TmLetIn (_, _, _) -> 6
;;

let string_of_term term =
  let rec clean_newlines s =
    let clean = Str.global_replace (Str.regexp "\n *\n") "\n" s in
    if clean = s then clean else clean_newlines clean
  in
  let rec internal indent outer term =
    let inner = term_precedence term in
    let result =
      (if indent then "\n" else "") ^
      (if inner >= outer then "(" else "") ^
      (match term with
          TmTrue ->
            "true"
        | TmFalse ->
            "false"
        | TmUnit ->
            "()"
        | TmZero ->
            "0"
        | TmVar s ->
            s
        | TmStr s ->
            "\"" ^ s ^ "\""
        | TmSucc t ->
            let rec f n t' = match t' with
                TmZero -> string_of_int n
              | TmSucc s -> f (n + 1) s
              | _ -> "succ " ^ internal false inner t
            in f 1 t
        | TmPred t ->
            "pred " ^ internal false inner t
        | TmIsZero t ->
            "iszero " ^ internal false inner t
            | TmPrintNat t ->
            "print_nat " ^ internal false inner t
        | TmPrintString t ->
            "print_string " ^ internal false inner t
        | TmPrintNewline t ->
            "print_newline " ^ internal false inner t
        | TmReadNat t ->
            "read_nat " ^ internal false inner t
        | TmReadString t ->
            "read_string " ^ internal false inner t
        | TmIf (t1,t2,t3) ->
            "if" ^
              internal true inner t1 ^
            "then" ^
              internal true inner t2 ^
            "else" ^
              internal true inner t3
        | TmAbs (s, tyS, t) ->
            "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^
              internal true inner t
        | TmFix (t1) ->
            "fix " ^ internal false inner t1
        | TmApp (t1, t2) ->
            internal false (inner + 1) t1 ^ " " ^ internal false inner t2
        | TmLetIn (s, t1, t2) ->
            "let " ^ s ^ " = " ^
              internal true inner t1 ^
            "in" ^
              internal true inner t2
        | TmTuple terms ->
            let len = List.length terms in
            let rec tuple_str t acc =
              match t with 
                  hd :: [] -> acc ^ (internal false inner hd) ^ (if len > 1 then ")" else ",)")
                | hd :: tl -> tuple_str tl (acc ^ (internal false inner hd) ^ ", ")
                | [] -> acc ^ (if len > 1 then ")" else ",)")
            in tuple_str terms "("
        | TmRecord entries ->
            let rec record_str t acc =
              match t with
                  (name, rt) :: [] -> acc ^ name ^ ": " ^ (internal false inner rt) ^ "}"
                | (name, rt) :: tl -> record_str tl (acc ^ name ^ ": " ^ (internal false inner rt) ^ ", ")
                | [] -> acc ^ "}"
            in record_str entries "{"
        | TmAccess (t, n) ->
            internal false inner t ^ "." ^ string_of_int n
        | TmAccessNamed (t, n) ->
            internal false inner t ^ "." ^ n
        | TmCons (ty, t1, t2) ->
            "cons[" ^ string_of_ty ty ^ "] " ^ internal false inner t1 ^ " " ^ internal false inner t2
        | TmNil ty ->
            "nil[" ^ string_of_ty ty ^ "]"
        | TmIsNil (ty, t) ->
            "isnil[" ^ string_of_ty ty ^ "] " ^ internal false inner t
        | TmHead (ty, t) ->
            "head[" ^ string_of_ty ty ^ "] " ^ internal false inner t
        | TmTail (ty, t) ->
            "tail[" ^ string_of_ty ty ^ "] " ^ internal false inner t
      )
    in (if indent then Str.global_replace (Str.regexp_string "\n") "\n  " result else result) ^
       (if indent then "\n" else "") ^
       (if inner >= outer then (if indent then "  )" else ")") else "")
  in clean_newlines (internal false 9999 term)
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

(* Free vars now needs context to take into account globals *)
let free_vars ctx tm =
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
    | TmStr s ->
        [s]
    | TmTuple terms -> 
        List.fold_left lunion [] (List.rev_map free_vars terms)
    | TmRecord entries ->
        List.fold_left lunion [] (List.rev_map (function (_, x) -> free_vars x) entries)
    | TmAccess (t, n) ->
        free_vars t
    | TmAccessNamed (t, n) ->
        free_vars t
    | TmCons (ty, t1, t2) ->
        lunion (free_vars t1) (free_vars t2)
    | TmNil ty ->
        []
    | TmIsNil (ty, t) ->
        free_vars t
    | TmHead (ty, t) ->
        free_vars t
    | TmTail (ty, t) ->
        free_vars t
  in lunion (free_vars tm) (List.map (function (s, _, _) -> s) ctx)
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
(* Subst now needs context to take into account globals as free vars *)
let rec subst ctx x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmUnit ->
      TmUnit
  | TmIf (t1, t2, t3) ->
      TmIf (subst ctx x s t1, subst ctx x s t2, subst ctx x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst ctx x s t)
  | TmPred t ->
      TmPred (subst ctx x s t)
  | TmIsZero t ->
      TmIsZero (subst ctx x s t)
  | TmPrintNat t ->
      TmPrintNat (subst ctx x s t)
  | TmPrintString t ->
      TmPrintString (subst ctx x s t)
  | TmPrintNewline t ->
      TmPrintNewline (subst ctx x s t)
  | TmReadNat t ->
      TmReadNat (subst ctx x s t)
  | TmReadString t ->
      TmReadString (subst ctx x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars ctx s in
          if not (List.mem y fvs)
          then TmAbs (y, tyY, subst ctx x s t)
          else let z = fresh_name y (free_vars ctx t @ fvs) in
                TmAbs (z, tyY, subst ctx x s (subst ctx y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst ctx x s t1, subst ctx x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst ctx x s t1, t2)
      else let fvs = free_vars ctx s in
          if not (List.mem y fvs)
          then TmLetIn (y, subst ctx x s t1, subst ctx x s t2)
          else let z = fresh_name y (free_vars ctx t2 @ fvs) in
                TmLetIn (z, subst ctx x s t1, subst ctx x s (subst ctx y (TmVar z) t2))
  | TmFix (t1) ->
      TmFix (subst ctx x s t1)
  | TmStr s ->
      TmStr s
  | TmTuple terms ->
      TmTuple (List.map (subst ctx x s) terms)
  | TmRecord entries ->
      TmRecord (List.map (function (n, t) -> (n, subst ctx x s t)) entries)
  | TmAccess (t, n) ->
      TmAccess (subst ctx x s t, n)
  | TmAccessNamed (t, n) ->
      TmAccessNamed (subst ctx x s t, n)
  | TmCons (ty, t1, t2) ->
      TmCons (ty, subst ctx x s t1, subst ctx x s t2)
  | TmNil ty ->
      TmNil ty
  | TmIsNil (ty, t) ->
      TmIsNil (ty, subst ctx x s t)
  | TmHead (ty, t) ->
      TmHead (ty, subst ctx x s t)
  | TmTail (ty, t) ->
      TmTail (ty, subst ctx x s t)
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
  | TmStr _ -> true
  | TmTuple _ -> true
  | TmRecord _ -> true
  | TmCons (_, _, _) -> true
  | TmNil _ -> true
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
      (try ignore (eval1 ctx t1) with NoRuleApplies -> ());
      print_string (string_of_term t1);
      TmUnit

    (* E-PrintString *)
  | TmPrintString t1 ->
      (try ignore (eval1 ctx t1) with NoRuleApplies -> ());
      print_string (match t1 with TmStr s -> s
                    | _ -> raise (Type_error "argument of print_string is not a string"));
      TmUnit

    (* E-PrintNewline *)
  | TmPrintNewline t1 ->
      (try ignore (eval1 ctx t1) with NoRuleApplies -> ());
      print_string "\n";
      TmUnit

    (* E-ReadNat *)
  | TmReadNat t1 ->
      (try ignore (eval1 ctx t1) with NoRuleApplies -> ());
      int_to_nat (read_int ())

    (* E-ReadString *)
  | TmReadString t1 ->
      (try ignore (eval1 ctx t1) with NoRuleApplies -> ());
      TmStr (read_line ())

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst ctx x v2 t12

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
      subst ctx x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst ctx x tm t2

    (* E-Fix *)
  | TmFix (t1) ->
      let t1' = eval1 ctx t1 in
      TmFix(t1')

    (* E-Var *)
  | TmVar (y) ->
      (try (match getbinding ctx y with (_, _, value) -> value) with
      _ -> raise (Type_error ("no binding value for variable " ^ y)))
      
    (* E-Access *)
  | TmAccess (t, n) ->
      (match t with
          TmTuple terms -> (
            let rec access_eval t i =
              match t with
                  hd :: tl -> if i = n then hd else access_eval tl (i + 1)
                | [] -> raise (Type_error "Tuple projection out of bounds")
            in access_eval terms 1
          )
        | _ -> raise (Type_error "Can only apply numbered projection to a tuple"))
  
    (* E-AccessNamed *)
  | TmAccessNamed (t, n) ->
      (match t with
          TmRecord entries -> (
            let rec access_named_eval t =
              match t with
                  (name, rt) :: tl -> if name = n then rt else access_named_eval tl
                | [] -> raise (Type_error "Record projection didn't match")
            in access_named_eval entries
          )
        | _ -> raise (Type_error "Can only apply named projection to a record"))

    (* E-IsNil *)
  | TmIsNil (ty, t) ->
      (match t with
          TmNil _ -> TmTrue
        | _ -> TmFalse)

    (* E-Head *)
  | TmHead (ty, t) ->
      (match t with
          TmCons (lty, t1, t2) -> t1
        | TmNil lty -> raise (Type_error ("Can't get head of Nil"))
        | _ -> raise (Type_error ("Argument of head[" ^ string_of_ty ty ^ "] must be a list[" ^ string_of_ty ty ^ "]")))

    (* E-Head *)
  | TmTail (ty, t) ->
      (match t with
          TmCons (lty, t1, t2) -> t2
        | TmNil lty -> TmNil lty
        | _ -> raise (Type_error ("Argument of tail[" ^ string_of_ty ty ^ "] must be a list[" ^ string_of_ty ty ^ "]")))

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
    subst ctx x s tm
  | _ -> tm
;;

(* Executes a command, returning the updated context *)
let run_cmd ctx = function
    CmdTerm (tm) -> 
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval ctx tm) ^ "\n: " ^ string_of_ty tyTm);
      ctx
  | CmdBind (x, bind) ->
      let bind = eval ctx bind in (* Evaluate whatever we can *)
      let bind = substall ctx bind in (* Replace with current context to ensure "functional-like" globals *)
      addbinding ctx x (typeof ctx bind) bind
;;

let check_cmd ctx = function
    CmdTerm (tm) -> 
      ignore (typeof ctx tm); ctx
  | CmdBind (x, bind) ->
      let bind = substall ctx bind in (* Replace with current context to ensure "functional-like" globals *)
      addbinding ctx x (typeof ctx bind) bind
;;

let run_cmd_silent ctx = function
    CmdTerm (tm) -> 
      ignore (eval ctx tm); ctx
  | CmdBind (x, bind) ->
      let bind = eval ctx bind in (* Evaluate whatever we can *)
      let bind = substall ctx bind in (* Replace with current context to ensure "functional-like" globals *)
      addbinding ctx x (typeof ctx bind) bind
;;