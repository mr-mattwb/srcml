open Unix
open Printf
open Pervasives

open IniBase

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
module type DEFAULT = 
    sig
        type elt 
        val default : elt
    end 
module type VAR = 
    sig
        include DEFAULT
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
module type VAR_FILELIST = sig include BVAR val default : Utils.file list end
module type VAR_DIRLIST = sig include BVAR val default : Utils.dir list end
module type VAR_STRLIST = sig include BVAR val default : string list end
module type VAR_SEC = sig include BVAR val default : Utils.sec end
module type VAR_MSEC = sig include BVAR val default : Utils.msec end
module type VAR_INET_ADDR = sig include BVAR val default : Unix.inet_addr end
module type VAR_SOCKADDR = sig include BVAR val default : Unix.sockaddr end

module type ELT = 
    sig
        include VAR
        val get : unit -> elt
        val put : elt -> unit
        val arg : spec
        val register : unit -> unit
        val of_str : string -> unit
        val to_str : unit -> string
        val name : unit -> var
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
module type DEBUG = 
    sig
        include FLAG
        val dprintf : ('a, unit, string, unit) format4 -> 'a
    end
module type DIR = 
    sig
        include ELT with type elt = Utils.dir
        val dir : unit -> Utils.dir
        val dir_ex : unit -> Utils.dir option
        val exists : unit -> bool
        val make : Unix.file_perm -> Utils.dir option
    end
module type FILE = 
    sig
        include ELT with type elt = Utils.file
        val file : unit -> Utils.file
        val file_ex : unit -> Utils.file option
        val exists : unit -> bool
        val touch : unit -> Utils.file option
    end
module type STRLIST = ELT with type elt = string list 
module type SEC = ELT with type elt = Utils.sec
module type MSEC = ELT with type elt = Utils.msec

module type SOCK_ELT = 
    sig
        include ELT with type elt = Unix.sockaddr
        val as_string : unit -> string
        val string_part : unit -> string
    end

module type INET_ADDR = ELT with type elt = Unix.inet_addr
module type SOCKADDR = 
    sig
        include ELT with type elt = Unix.sockaddr
        val as_string : unit -> string
        val string_part : unit -> string
        val sock_type : unit -> socket_domain
        val sock_stream : unit -> Unix.file_descr
    end

module type REX = 
    sig
        include STR
        val rex : unit -> Pcre.regexp
        val split : string -> string list
        val replace : string -> string -> string
        val matches : string -> bool
    end

module type DIRLIST = ELT with type elt = Utils.dir list
module type FILELIST = ELT with type elt = Utils.file list
module type DIRSET = 
    sig
        include ELT with type elt = Utils.DirSet.t 
        val to_list : unit -> Utils.dir list
        val add : Utils.dir -> unit
        val add_list : Utils.dir list -> unit
    end
module type FILECFG = 
    sig
        include ELT with type elt = Utils.file
        val file : unit -> elt option
        val files : unit -> elt list
        val parse_file : unit -> elt option
        val parse_files : unit -> elt list
    end

module type ENV = 
    sig
        val from_lex : Lexing.lexbuf -> unit
        val from_string : string -> unit
        val from_channel : in_channel -> unit
        val from_file : Utils.file -> unit

        val default_ctx : context
        val get : ?ctx:context -> var -> value option
        val put : ?ctx:context -> var -> value -> unit
        val find : ?ctx:context -> var -> value -> value
        val del : ?ctx:context -> var -> unit
        val add : ?ctx:context -> var -> value -> unit
        val parse : Utils.file -> unit
        val update : context -> var -> value -> unit
    end
module type PARAMS =
    sig
        val register : spec -> unit

        val def_invalid_arg : value
        val env_invalid_arg : var
        val invalid_arg : value

        val param_list : unit -> spec list
        val parse_cli_params : unit -> string list
        val parse_params : unit -> string list
    end
module type LOAD =
    sig
        val load : (unit -> Utils.file) -> string list
        val load_option : (unit -> Utils.file option) -> string list
        val load_all : (unit -> Utils.file list) -> string list

        val load_params : unit -> string list

        val load_file : Utils.file -> unit
        val load_file_option :  Utils.file option -> unit
        val load_files : Utils.file list -> unit
    end
module type LOADCFG =
    sig
        include FILECFG
        val loader : (unit -> unit) -> string list
        val load_file : unit -> string list
        val load_files : unit -> string list
        val load_params : unit -> string list
        val load : unit -> string list
    end

module type ICFG =
    sig
        val cfg : IniBase.cfg
    end

module type CFG = 
    sig
        module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) : ELT with type elt = S.elt
        module MakeDefault(S : Ser.ELT)(D : DEFAULT with type elt = S.elt)(V : BVAR) : ELT with type elt = D.elt
        module Hide(Elt : ELT) : ELT with type elt = Elt.elt
        module Signal(V : VAR_STR) : INT
        module ISignal(V : VAR_INT) : INT
        module Str(V : VAR_STR) : STR
        module StrEmpty(V : BVAR) : STR
        module Int(V : VAR_INT) : INT
        module Int0(V : BVAR) : INT
        module Int32(V : VAR_INT32) : INT32
        module Int64(V : VAR_INT64) : INT64
        module Flt(V : VAR_FLT) : FLT
        module Flt0(V : BVAR) : FLT
        module Char(V : VAR_CHAR) : CHAR
        module Byte(V : VAR_BYTES) : BYTES
        module Bool(V : VAR_BOOL) : BOOL
        module Flag(V : VAR_FLAG)(D : sig val default : bool end) : BOOL
        module Set(V : VAR_FLAG) : FLAG
        module Clear(V : VAR_FLAG) : FLAG
        module Dir(V : VAR_DIR) : DIR
        module File(V : VAR_FILE) : FILE
        module Sec(V : VAR_SEC) : SEC
        module MSec(V : VAR_MSEC) : MSEC

        module InetAddr(V : VAR_INET_ADDR) : INET_ADDR
        module Hostname(V : VAR_STR) : INET_ADDR 
        module SockAddr(V : VAR_SOCKADDR) : SOCKADDR
        module SockAddress(V : VAR_SOCKADDR) : SOCKADDR
        module SockAny(V : BVAR) : SOCKADDR

        module Rex(V : VAR_STR) : REX

        module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) :
            ELT with type elt = S.elt option
        module OptionNone(S : Ser.ELT)(V : BVAR) : VAR with type elt = S.elt option

        module MakeList(Sep : Ser.SEP)(E : Ser.ELT)(V : VAR with type elt = E.elt list) :
            ELT with type elt = E.elt list
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

        module DirList(V : VAR_DIRLIST) : DIRLIST
        module FileList(V : VAR_FILELIST) : FILELIST
        module DirSet(V : BVAR) : DIRSET

        module Debug : 
            sig
                include FLAG
                val dprintf : ('a, unit, string, unit) format4 -> 'a
            end

        module FileCfg(Dirs : DIRLIST)(File : FILE) : FILECFG

        module CfgPath : DIRLIST           (* CFG_PATH  --cfg-path  [] *)
        module CfgFile : FILE              (* CFG_FILE  --cfg-file  .Sys.argv.(0) *)
        module ConfigPath(Dirs : DIRLIST)(F : FILE) : FILECFG
        module ConfigFile(F : FILE) : FILECFG
        module LoadCfg(F : FILECFG) : LOADCFG
        module Configure(F : FILE) : LOADCFG
    end

module Load(Env : ENV)(Params : PARAMS) : LOAD
module Params : PARAMS

val make_desc : context -> env -> switch -> desc -> desc

module Make(Env : ENV)(Params : PARAMS) : CFG
module MakeStd(E : ENV) : CFG
