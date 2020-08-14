open Unix
open Printf
open Pervasives

exception Conversion of string * string

class type ['a] elt =
    object
        method of_str : string -> 'a
        method to_str : 'a -> string
    end

class type sep_elt = 
    object
        method rex : Pcre.regexp
        method sep : string
    end

class ['a] t : (string -> 'a) -> ('a -> string) -> 
    object
        inherit ['a] elt
    end

(*
val ser_str : string elt
val ser_int : int elt
val ser_int32 : int32 elt
val ser_int64 : int64 elt
val ser_flt : float elt
val ser_chr : char elt
val ser_bool : bool elt
val ser_bytes : bytes elt

val debug_var : string
val debug : bool
val dbg_printf : ('a, unit, string, unit) format4 -> 'a
val e_printf : string -> exn -> ('a, unit, string, unit) format4 -> 'a

val ser_inet : inet_addr elt
val ser_saddr : sockaddr elt

val make_list : 'a elt -> sep_elt -> 'a list elt

val wspc : string
val rex_wspc : string -> Pcre.regexp
val rex_space : Pcre.regexp

class sep_t : Pcre.regexp -> string -> sep_elt
class sep_wspc : string -> string -> sep_elt
class sep_str_wspc : string -> sep_elt

val comma : sep_elt
val colon : sep_elt
val semicolon : sep_elt
val bar : sep_elt
val space : sep_elt

val wspc_sep_list : 'a elt -> string -> string -> 'a list elt
val wspc_str_list : 'a elt -> string -> 'a list elt
val comma_list : 'a elt -> 'a list elt
val colon_list : 'a elt -> 'a list elt
val semicolon_list : 'a elt -> 'a list elt
val bar_list : 'a elt -> 'a list elt
val space_list : 'a elt -> 'a list elt

val make_tuple : sep_elt -> 'a elt -> 'b elt -> ('a * 'b) elt
val comma_tuple : 'a elt -> 'b elt -> ('a * 'b) elt
val colon_tuple : 'a elt -> 'b elt -> ('a * 'b) elt
val space_tuple : 'a elt -> 'b elt -> ('a * 'b) elt
*)


