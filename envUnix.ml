open Unix
open Printf
open Pervasives

open IniBase
open EnvCfg

module IEnv(C : ICFG) : ENV =
    struct
        let gCfg = ref C.cfg
        let from_lex lex = gCfg := IniLex.process lex !gCfg
        let from_string str = from_lex (Lexing.from_string str)
        let from_channel fin = from_lex (Lexing.from_channel fin)
        let from_file fn = Utils.file_in from_channel fn

        let unix_getenv ctx var = 
            try Some (Unix.getenv (context_name ctx var))
            with Not_found -> None
        let unix_findenv ctx var def =
            try Unix.getenv (context_name ctx var)
            with Not_found -> def

        let default_ctx = IniBase.default_context
        let get ?(ctx=default_ctx) var = 
            match IniBase.get_env ~ctx var !gCfg with
            | None -> unix_getenv ctx var
            | sv -> sv
        let put ?(ctx=default_ctx) var v = 
            gCfg := IniBase.put_env ~ctx var v !gCfg
        let find ?(ctx=default_ctx) var def = 
            try IniBase.find_env ~ctx var !gCfg
            with Not_found -> unix_findenv ctx var def
        let del ?(ctx=default_ctx) var = 
            gCfg := IniBase.rem_env ~ctx var !gCfg
        let add ?(ctx=default_ctx) var s = 
            gCfg := IniBase.put_env ~ctx var s !gCfg
        let parse fname = from_file fname 
        let update ctx env def = 
            match get ~ctx env with
            | None -> add ~ctx env def
            | Some s -> add ~ctx env s
    end

module UEnv : ENV =
    struct
        let from_lex lex = IniLex.uprocess lex
        let from_string str = from_lex (Lexing.from_string str)
        let from_channel fin = from_lex (Lexing.from_channel fin)
        let from_file fname = Utils.file_in from_channel fname

        let default_ctx = default_context
        let get ?(ctx=default_ctx) var = 
            try 
                match Unix.getenv (context_name ctx var) with
                | "" -> None
                | s -> Some s
            with Not_found -> None
        let put ?(ctx=default_ctx) var dv = 
            Unix.putenv (context_name ctx var) dv
        let find ?(ctx=default_ctx) var def = 
            try Unix.getenv (context_name ctx var)
            with Not_found -> def
        let del ?(ctx=default_ctx) var = 
            match get ~ctx var with
            | None 
            | Some "" -> ()
            | Some _ -> put ~ctx var ""
        let add ?(ctx=default_ctx) var v = 
            Unix.putenv (context_name ctx var) v
        let parse fname = from_file fname

        let update ctx env def = 
            match get ~ctx env with
            | None -> put ~ctx env def
            | Some _ -> ()
    end

module UCfg = MakeStd(UEnv)
include UCfg


