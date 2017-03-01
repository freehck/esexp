open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.main Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec to_string = function
  | `List [] -> "()"
  | `List vals -> List.map ~f:to_string vals
		  |> String.concat ~sep:" "
		  |> Printf.sprintf "(%s)"
  | `String s -> Printf.sprintf "\"%s\"" s
  | `Symbol s -> s
  | `Int num -> Int.to_string num
  | `Float num -> Float.to_string num

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
     print_endline @@ to_string value;
     parse_and_print lexbuf
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let () =
  loop "input.example" ()
