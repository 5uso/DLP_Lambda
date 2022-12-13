
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(* Reads lines from channel until it finds the end of line indicator ;; *)
let read_multiline channel = 
  let is_end s1 =
    let exp = (Str.regexp_string ";;") in
    try ignore (Str.search_forward exp s1 0); true
    with Not_found -> false
  in let rec internal curr =
    let s = input_line channel in
    let concat = curr ^ "\n" ^ s in
    if is_end s then concat else internal concat
  in internal "";;

(* Checks for typing errors in every expression, returns true if it reaches EOF *)
let check input =
  let rec check_loop ctx line =
    try
      let c = s token (from_string (read_multiline input)) in
      let ctx = check_cmd ctx c in (* Commands may update the global context *)
      check_loop ctx (line + 1) (* Keep track of lineno to display if we run into an error *)
    with
      Lexical_error e ->
        Printf.printf "Lexical error in line %d: %s\n" line e; false
    | Parse_error ->
        Printf.printf "Syntax error in line %d\n" line; false
    | Type_error(e) ->
        Printf.printf "Type error in line %d: %s\n" line e; false
    | End_of_file -> true (* No typing errors found, execute *)
  in check_loop emptyctx 1 (* Start at line 1 with empty context *)
;;

(* Runs the file, doesn't expect errors *)
let run input =
  let rec run_loop ctx =
    try
      let c = s token (from_string (read_multiline input)) in
      let ctx = run_cmd_silent ctx c in (* Commands may update the global context *)
      run_loop ctx
    with
      End_of_file -> ()
  in run_loop emptyctx (* Start with empty context *)
;;

try
  let filename = Sys.argv.(1) in (* Filename to run is the first commandline argument after the executable path *)
  if check (open_in filename) then run (open_in filename) else () (* Only run if "compilation" (typeof) succeeded *)
with Invalid_argument(_) -> print_endline "Usage: run <source_file>"
| Sys_error(err) -> print_endline ("Couldn't open file " ^ err);;
