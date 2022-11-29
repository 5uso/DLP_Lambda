
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let read_multiline () = 
  let is_end s1 =
    let exp = (Str.regexp_string ";;") in
    try ignore (Str.search_forward exp s1 0); true
    with Not_found -> false
  in let rec internal curr =
    let s = read_line () in
    let concat = curr ^ "\n" ^ s in
    if is_end s then concat else internal concat
  in internal "";;

let top_level_loop () =
  print_endline "Lambda expression evaluator...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let tm = s token (from_string (read_multiline ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

