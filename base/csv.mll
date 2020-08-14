{
open Unix
open Printf
open Pervasives
}
let quoted = '"' ([^ '"']* as item) '"'
let item = ([^ ',']* as item)
rule line = parse
    | quoted        { item :: (rest lexbuf) }
    | item          { item :: (rest lexbuf) }
    | eof           { [] }
and rest = parse
    | ',' quoted        { item :: (rest lexbuf) }
    | ',' item          { item :: (rest lexbuf) }
    | ',' quoted eof    { item :: [] }
    | ',' item eof      { item :: [] }
    | ',' eof           { "" :: [] }
