open Unix

open Printf
open Pervasives

val from_lex : Lexing.lexbuf -> unit
val from_string : string -> unit
val from_channel : in_channel -> unit
val from_file : Utils.file -> unit

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
        val put : elt -> unit
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
        val dir_ex : unit -> Utils.dir option
        val exists : unit -> bool
    end
module type FILE = 
    sig
        include ELT with type elt = Utils.file
        val file : unit -> Utils.file
        val file_ex : unit -> Utils.file option
        val exists : unit -> bool
    end
module type STRLIST = ELT with type elt = string list
module type SOCKADDR = ELT with type elt = Unix.sockaddr
module type SEC = ELT with type elt = Utils.sec
module type MSEC = ELT with type elt = Utils.msec

val default_ctx : context
val env_name : context -> env -> string

val getenv : ?ctx:context -> env -> string option
val putenv : ?ctx:context -> env -> string -> unit
val findenv : ?ctx:context -> env -> string -> string
val remenv : ?ctx:context -> env -> unit
val parse : Utils.file -> unit

val register : spec -> unit
val param_list : unit -> spec list
val parse_params : unit -> string list
val load_config : (unit -> Utils.file) -> string list
val load_file : Utils.file -> unit

val make_desc : context -> env -> switch -> desc -> desc

module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) : ELT with type elt = V.elt
module Str(V : VAR_STR) : STR
module StrEmpty(V : BVAR) : STR
module Int(V : VAR_INT) : INT
module Int0(V : BVAR) : INT
module Int32(V : VAR_INT32) : INT32
module Int64(V : VAR_INT64) : INT64
module Flt(V : VAR_FLT) : FLT
module Flt0(V : BVAR) : FLT
module Char(V : VAR_CHAR) : CHAR
module Bytes(V : VAR_BYTES) : BYTES
module Bool(V : VAR_BOOL) : BOOL
module Flag(V : VAR_FLAG)(D : sig val default : bool end) : BOOL
module Set(V : VAR_FLAG) : FLAG
module Clear(V : VAR_FLAG) : FLAG
module Dir(V : VAR_DIR) : DIR
module File(V : VAR_FILE) : FILE
module Sec(V : VAR_SEC) : SEC
module MSec(V : VAR_MSEC) : MSEC

module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) : ELT with type elt = V.elt
module OptionNone(S : Ser.ELT)(V : BVAR) : ELT with type elt = S.elt option

module MakeList(Sep : Ser.SEP)(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
module ListNull(Sep : Ser.SEP)(E : Ser.ELT)(V : BVAR) : ELT with type elt = E.elt list

module CommaList(S : Ser.ELT)(V : VAR with type elt = S.elt list) : ELT with type elt = S.elt list
module SpaceList(S : Ser.ELT)(V : VAR with type elt = S.elt list) : ELT with type elt = S.elt list
module ColonList(S : Ser.ELT)(V : VAR with type elt = S.elt list) : ELT with type elt = S.elt list
module SemiColonList(S : Ser.ELT)(V : VAR with type elt = S.elt list) : ELT with type elt = S.elt list
module BarList(S : Ser.ELT)(V : VAR with type elt = S.elt list) : ELT with type elt = S.elt list

module StrList(Sep : Ser.SEP)(V : VAR_STRLIST) : STRLIST
module CommaStrList(V : VAR_STRLIST) : STRLIST
module SpaceStrList(V : VAR_STRLIST) : STRLIST
module ColonStrList(V : VAR_STRLIST) : STRLIST
module SemiColonStrList(V : VAR_STRLIST) : STRLIST
module BarStrList(V : VAR_STRLIST) : STRLIST

module SockAddr(V : VAR_SOCKADDR) : SOCKADDR
module SockAny(V : BVAR) : SOCKADDR

module type DIRLIST = ELT with type elt = Utils.dir list
module type FILELIST = ELT with type elt = Utils.file list
module type FILECFG =
    sig
        include ELT with type elt = Utils.file
        val file : unit -> elt option
        val files : unit -> elt list
        val parse_file : unit -> elt option
        val parse_files : unit -> elt list
    end

module DirList(V : BVAR) : DIRLIST
module FileList(V : BVAR) : FILELIST 

module Config(D : DIRLIST)(V : BVAR) : FILECFG

module CfgDirsVar : BVAR   (* CFG_PATH --cfg-path *)
module CfgFileVar : BVAR   (* CFG_FILE --cfg-file *)

module ConfigFile(D : BVAR)(F : BVAR) : FILECFG
module CfgPath(F : BVAR) : FILECFG

