(*
  Lambda Top Level,
  by Jesús Mosteiro García and Luca D’angelo Sabin
    (jesus.mosteiro@udc.es)    (l.dangelo@udc.es)

  Made for the programming language design course at UDC.
*)

open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;
open List;;

(* Reads lines until it finds the end of line indicator ;; *)
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
      (* Get and run a command *)
      let c = s token (from_string (read_multiline ())) in
      let ctx = run_cmd ctx c in (* Commands may update the global context *)
      loop ctx
    with
       Lexical_error e -> (* For clarity, lexer errors display the unexpected input *)
         print_endline ("lexical error: " ^ e);
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
    loop emptyctx (* Start with empty context *)
  ;;

top_level_loop ()
;;

