{
  open Printf
  open Pervasives
  exception Code_change_unexpected of string * string

  let concat code line code' line' = 
    if code = code' then
      line^"\n"^line'
    else
      raise (Code_change_unexpected (code, code'))
}
let digit = ['0'-'9']
let cr = '\r'
let lf = '\n'
let crlf = cr lf
let contch = '-'
let space = ' '
let not_crlf = ([^ '\r'] | '\r' [^ '\n'])
let not_space = [^ ' ']
let line = not_crlf* crlf

let code = digit digit digit

rule token = parse
  | code as c              { (c, continue c lexbuf) }

and continue c = parse
  | contch (line as l)   
      { 
        let (c', line) = token lexbuf in
        concat c l c' line
      }
  | line
      { 
        Lexing.lexeme lexbuf 
      }

