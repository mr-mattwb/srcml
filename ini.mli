open Unix
open Printf
open Pervasives

open IniBase

val empty : cfg

val add_lex : Lexing.lexbuf -> cfg -> cfg
val add_string : string -> cfg -> cfg
val add_channel : in_channel -> cfg -> cfg
val add_file : Utils.file -> cfg -> cfg

val init_lex : Lexing.lexbuf -> cfg
val init_string : string -> cfg
val init_channel : in_channel -> cfg
val init_file : Utils.file -> cfg

val unix_lex : ?ctx:context -> Lexing.lexbuf -> cfg
val unix_string : ?ctx:context -> string -> cfg
val unix_channel : ?ctx:context -> in_channel -> cfg
val unix_file : ?ctx:context -> Utils.file -> cfg

(*
-----------------------------------
*)

type context = IniBase.context
type env = string
type switch = string
type desc = string
type spec = switch * Arg.spec * desc


module type BVAR = 
    sig
        val context : context
        val env : env
        val switch : switch 
        val desc : desc
    end
module type VAR = 
    sig
        type elt
        val default : elt
        include BVAR
    end
module type VAR_STR = sig include BVAR val default : string end
module type VAR_INT = sig include BVAR val default : int end
module type VAR_INT32 = sig include BVAR val default : int32 end
module type VAR_INT64 = sig include BVAR val default : int64 end
module type VAR_FLT = sig include BVAR val default : float end
module type VAR_CHAR = sig include BVAR val default : char end
module type VAR_BYTES = sig include BVAR val default : bytes end
module type VAR_BOOL = sig include BVAR val default : bool end
module type VAR_FLAG = BVAR
module type VAR_DIR = sig include BVAR val default : Utils.dir end
module type VAR_FILE = sig include BVAR val default : Utils.file end
module type VAR_STRLIST = sig include BVAR val default : string list end
module type VAR_SOCKADDR = sig include BVAR val default : Unix.sockaddr end
module type VAR_SEC = sig include BVAR val default : Utils.sec end
module type VAR_MSEC = sig include BVAR val default : Utils.msec end

module type ELT = 
    sig
        include VAR
        val get : unit -> elt
        val set : elt -> unit
        val arg : spec
    end
module type STR = ELT with type elt = string
module type INT = ELT with type elt = int
module type INT32 = ELT with type elt = int32
module type INT64 = ELT with type elt = int64
module type FLT = ELT with type elt = float
module type CHAR = ELT with type elt = char
module type BYTES = ELT with type elt = bytes
module type BOOL = ELT with type elt = bool
module type FLAG = BOOL
module type DIR = 
    sig
        include ELT with type elt = Utils.dir
        val dir : unit -> Utils.dir
        val exists : unit -> bool
    end
module type FILE = 
    sig
        include ELT with type elt = Utils.file
        val file : unit -> Utils.file
        val exists : unit -> bool
    end
module type STRLIST = ELT with type elt = string list
module type SOCKADDR = ELT with type elt = Unix.sockaddr
module type SEC = ELT with type elt = Utils.sec
module type MSEC = ELT with type elt = Utils.msec

module type SIG = 
    sig
        val pIter : context -> (var -> value -> unit) -> props -> unit
        val cIter : (context -> props -> unit) -> unit

        val getenv : ?ctx:context -> var -> value option
        val putenv : ?ctx:context -> var -> value -> unit
        val findenv : ?ctx:context -> var -> value
        val remenv : ?ctx:context -> var -> unit
        val addenv : ?ctx:context -> var -> value -> unit
        val parse_file : Utils.file -> unit

        val buffer : Buffer.t -> unit
        val to_string : unit -> string
        val output : out_channel -> unit
        val print : unit -> unit

        val register : spec -> unit
        val param_list : unit -> spec list
        val parse_params : unit -> string list
        val load_config : (unit -> Utils.file) -> string list
        val load_file : Utils.file -> unit
        val load_unix : context -> unit

        module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) : ELT with type elt =V.elt
        module Str(V : VAR_STR) : STR
        module StrEmpty(V : BVAR) : STR
        module Int(V : VAR_INT) : INT
        module Int0(V : BVAR) : INT
        module Int32(V : VAR_INT32) : INT32
        module Int64(V : VAR_INT64) : INT64
        module Flt(V : VAR_FLT) : FLT
        module Char(V : VAR_CHAR) : CHAR
        module Bytes(V : VAR_BYTES) : BYTES
        module Bool(V : VAR_BOOL) : BOOL
        module Flag(V : VAR_FLAG)(D : sig val default : bool end) : FLAG
        module Set(V : VAR_FLAG) : FLAG
        module Clear(V : VAR_FLAG) : FLAG
        module Dir(V : VAR_DIR) : DIR
        module File(V : VAR_FILE) : FILE
        module Sec(V : VAR_SEC) : SEC
        module MSec(V : VAR_MSEC) : MSEC
        module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) : ELT with type elt = S.elt option
        module OptNone(S : Ser.ELT)(V : BVAR) : ELT with type elt = S.elt option
        module MakeList(Sep : Ser.SEP)(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = V.elt
        module MakeNull(Sep : Ser.SEP)(E : Ser.ELT)(V : BVAR) : ELT with type elt = E.elt list
        module StrList(Sep : Ser.SEP)(V : VAR_STRLIST) : STRLIST
        module CommaList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module SpaceList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module ColonList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module SemiColonList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module BarList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module CommaStrList(V : VAR_STRLIST) : STRLIST
        module ColonStrList(V : VAR_STRLIST) : STRLIST
        module SemiColonStrList(V : VAR_STRLIST) : STRLIST
        module SpaceStrList(V : VAR_STRLIST) : STRLIST
        module BarStrList(V : VAR_STRLIST) : STRLIST
        module SockAddr(V : VAR_SOCKADDR) : SOCKADDR
        module SockAny(V : BVAR) : SOCKADDR
    end

module type CTX = sig val ctx : IniBase.context end
module type INIT = sig val cfg : IniBase.cfg end

module Empty : INIT
module IUnix(Ctx : CTX) : INIT
module IFile(F : Cfg.FILE) : INIT

module IMake(C : sig val cfg : IniBase.cfg end) : SIG

include SIG   (* The [default] context *)

