
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

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

let check input =
  let rec check_loop ctx line =
    try
      let c = s token (from_string (read_multiline input)) in
      let ctx = check_cmd ctx c in
      check_loop ctx (line + 1)
    with
      Lexical_error ->
        Printf.printf "Lexical error in line %d\n" line; false
    | Parse_error ->
        Printf.printf "Syntax error in line %d\n" line; false
    | Type_error(e) ->
        Printf.printf "Type error in line %d: %s\n" line e; false
    | End_of_file -> true
  in check_loop emptyctx 1
;;

let run input =
  let rec run_loop ctx line =
    try
      let c = s token (from_string (read_multiline input)) in
      let ctx = run_cmd_silent ctx c in
      run_loop ctx (line + 1)
    with
      End_of_file -> ()
  in run_loop emptyctx 1
;;

try
  let filename = Sys.argv.(1) in
  if check (open_in filename) then run (open_in filename) else ()
with Invalid_argument(_) -> print_endline "Usage: run <source_file>"
| Sys_error(err) -> print_endline ("Couldn't open file " ^ err);;
