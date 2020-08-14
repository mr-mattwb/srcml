{
open Unix
open Printf
open Pervasives

open IniBase
open IniParse


let line_no = ref 1
let trim (str : string) = 
    let rec check pos = 
        if pos < 0 then ""
        else 
            match String.get str pos with
            |' '|'\t' -> check (pos-1)
            | _ -> String.sub str 0 (1+pos)
    in
    let len = String.length str in
    if len = 0 then ""
    else 
        match str.[len-1] with
        |' '|'\t' -> check (len-1)
        | _ -> str

}

let wspc = [' ' '\t' '\r' ]
let quoted = [^ '"' '\\']|'\\'[^'"']|'\\''"'
let cmt = (';'|'#')
let ttkn = [^ ' ' '\t' '[' ']' '"' ';' '=' '\r' '\n']
let tkn = [^ '[' ']' '"' ';' '=' '\r' '\n']

rule token = parse
    | wspc+                             { token lexbuf }
    | cmt [^'\n']*                      { token lexbuf }
    | '"' (quoted* as qu) '"'           { QUOTE qu }
    | '\n'                              { EOL }
    | '['                               { BCTX }
    | ']'                               { ECTX }
    | '='                               { EQ }
    | ttkn tkn*                         { TOKEN (trim (Lexing.lexeme lexbuf)) } 
    | eof                               { EOF }

{

let rec uprocess lex = 
    let rec loop ctx =
        match IniParse.main token lex with
        | Context nc -> 
            loop nc 
        | Pair (k, v) ->
            Unix.putenv (context_name ctx k) v;
            loop ctx
        | Eof -> 
            ()
    in
    loop ""

let rec process lex cfg = 
    let rec loop cmap ctx vmap =
        match IniParse.main token lex with
        | Context nc ->
            loop (Ctx.add ctx vmap cmap) nc Props.empty
        | Pair (k, v) ->
            loop cmap ctx (Props.add k v vmap)
        | Eof ->
            Ctx.add ctx vmap cmap
    in
    let ienv = 
        match get_props "" cfg with
        | None -> Props.empty
        | Some c -> c
    in
    loop cfg "" ienv
}

