open Parser

type token_attr = {
  lineno: int;
  cnum: int;
}

let line_number = ref 1
let last_token = ref EOF
let char_num = ref 1
let filename = ref ""

let build_token_list lexbuf =
  let rec helper prev_cnum prev_lineno lexbuf token_list =
    let token = Scanner.token lexbuf in
    let lineno = !Scanner.lineno in
    let cnum = (Lexing.lexeme_start_p lexbuf).Lexing.pos_cnum in
    let prev_cnum = if lineno > prev_lineno then cnum else prev_cnum in
    let cnum = cnum - prev_cnum in
    match token with
        EOF as eof -> (eof, { lineno = lineno; cnum = cnum } )::token_list
    |   t          -> (t, { lineno = lineno; cnum = cnum } )::(helper prev_cnum lineno lexbuf token_list)
  in helper 0 0 lexbuf []

let parser filen token_list =
  let token_list = ref(token_list) in
  let tokenizer _ =
    match !token_list with
      | (head, curr) :: tail -> 
          filename := filen;
          line_number := curr.lineno;
          char_num    := curr.cnum;
          last_token := head;
          token_list := tail;
          head
      | [] -> raise (Exceptions.MissingEOF)
  in
  let program = Parser.program tokenizer (Lexing.from_string "") in
  program