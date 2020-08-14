{
    open Buffer
    open Pcre
    let sdequote = 
        let rex = regexp "\\\\'" in
        fun s -> replace ~rex ~templ:"'" s
    let ddequote = 
        let rex = regexp "\\\\\"" in
        fun d -> replace ~rex ~templ:"\"" d
}
let ws = [' ' '\t' '\r' '\n']
let sq = '\''
let not_sq = ([^'\'']|"\\\'")
let dq = '"'
let not_dq = ([^'"']|"\\\"")

rule token = parse 
    | ws*                       { token lexbuf }
    | sq not_sq* eof            { `Text (Lexing.lexeme lexbuf) }
    | dq not_dq* eof            { `Text (Lexing.lexeme lexbuf) }
    | sq (not_sq* as s) sq      { `Quoted (sdequote s) }
    | dq (not_dq* as d) dq      { `Quoted (ddequote d) }
    | eof                       { `Eof }
    | ([^'\'' '"']+ as s)       { `Text s }

{
let unquote str =
    let buf = Buffer.create (String.length str) in
    let lex = Lexing.from_string str in
    let add s = Buffer.add_string buf s in
    let rec unq () =
        match token lex with
        | `Quoted s -> add s; unq ()
        | `Text txt -> add txt; unq ()
        | `Eof -> Buffer.contents buf
    in
    unq ()
}

