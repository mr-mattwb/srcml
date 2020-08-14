{
  open Buffer
}
let wspc = [' ' '\t']
rule token buf = parse
  | wspc*           { token buf lexbuf }
  | eof             { `Eof (Utils.contents buf) }
  | '='             { `Eq (Utils.contents buf) }
  | '#' _*          { `Eof (Utils.contents buf) }
  | '"' (([^ '"']|('\\' '"'))* as s)'"'
                    { let lex = Lexing.from_string s in
                      dquote buf lex;
                      token buf lexbuf }
  
  | ([^ '#' '"' '=' ' ' '\t']* as s)
                    { add_string buf s; token buf lexbuf }
and dquote buf = parse
  | eof               { () }
  | '\\' '"'          { add_char buf '"'; dquote buf lexbuf }
  | ([^ '\\' '"']* as s)   
                      { add_string buf s; dquote buf lexbuf }
  | (('\\' [^ '"']) as s)
                      { add_string buf s; dquote buf lexbuf }
