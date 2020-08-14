{
open Unix
open Printf
open Pervasives
}
let sep = '^'
let quoted = '"' ([^ '"']* as item) '"'
let item = ([^ '^']* as item)
rule line = parse
    | quoted        { item :: (rest lexbuf) }
    | item          { item :: (rest lexbuf) }
    | eof           { [] }
and rest = parse
    | sep quoted        { item :: (rest lexbuf) }
    | sep item          { item :: (rest lexbuf) }
    | sep quoted eof    { item :: [] }
    | sep item eof      { item :: [] }
    | sep eof           { "" :: [] }
