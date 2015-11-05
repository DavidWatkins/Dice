open Parser

let line_number = ref 1
let last_token = ref EOF

let build_token_list lexbuf =
  let rec helper lexbuf token_list =
    let token = Scanner.token lexbuf in
    let ln = !Scanner.lineno in
    match token with
        EOF as eof -> (eof, ln)::token_list
    |   t          -> (t, ln)::(helper lexbuf token_list)
  in helper lexbuf []

let parser token_list =
  let token_list = ref(token_list) in
  let tokenizer _ =
    match !token_list with
      | (head, ln) :: tail -> 
          line_number := ln;
          last_token := head;
          token_list := tail;
          head
      | [] -> raise (Exceptions.MissingEOF)
  in
  let program = Parser.program tokenizer (Lexing.from_string "") in
  program