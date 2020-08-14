open Unix
open Printf
open Pervasives

open Utils

type env = string
type var = string
type switch = string
type desc = string
type spec = switch * Arg.spec * desc

val is_hidden : env -> bool
val hide : env -> switch -> unit
val hiddens : unit -> env list

val iter : (env -> string -> unit) -> unit
val fold : (env -> string -> 'a -> 'a) -> 'a -> 'a

val env_list : unit -> env list
val env_get : env -> string * desc
val env_val : env -> string
val env_desc : env -> desc 
val env_iter : (env -> string * desc -> unit) -> unit

val get_args : unit -> spec list
val invalid_arg : string -> unit (* exits *)
val invalid_argument : string
val arg_parse : unit -> unit
val arg_parse_extra : unit -> string list
val output : out_channel -> unit
val output_file : file -> unit
val remove : env -> unit

module type BVAR = 
    sig
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

module type ELT =
  sig
    include VAR 
    val get : unit -> elt
    val set : elt -> unit
    val arg : spec
  end
module type STR = ELT with type elt = string
module type REX = 
    sig
        include STR
        val rex : unit -> Pcre.regexp
    end
module type CHR = ELT with type elt = char
module type INT = ELT with type elt = int
module type INT32 = ELT with type elt = int32
module type INT64 = ELT with type elt = int64
module type FLT = ELT with type elt = float
module type BOOL = ELT with type elt = bool
module type BYTES = ELT with type elt = bytes
module type INET = ELT with type elt = Unix.inet_addr
module type SADDR = ELT with type elt = Unix.sockaddr
module type HOST = ELT with type elt = host
module type PORT = ELT with type elt = port
module type EMAIL = ELT with type elt = email
module type EMAILS = ELT with type elt = email list

module Hide(Elt : ELT) : ELT with type elt = Elt.elt

module Make(C : Ser.ELT)(E : VAR with type elt = C.elt) : 
  ELT with type elt = E.elt

module type STR_VAR = 
  sig
    val env : env
    val default : string
    val switch : switch
    val desc : desc
  end
module Str(E : STR_VAR) : STR
module Rex(E : STR_VAR) : REX

module type CHR_VAR = 
    sig
        val env : env
        val default : char
        val switch : switch
        val desc : desc
    end
module Chr(E : CHR_VAR) : CHR

module type INT_VAR = 
  sig
    val env : env
    val default : int
    val switch : switch
    val desc : desc
  end
module Int(I : INT_VAR) : INT

module type INT0_VAR = 
  sig
    val env : env
    val switch : switch
    val desc : desc
  end
module Int0(I : INT0_VAR) : INT

module type INT32_VAR = 
  sig
    val env : env
    val default : int32
    val switch : switch
    val desc : desc
  end
module Int32(I : INT32_VAR) : INT32

module type INT64_VAR = 
  sig
    val env : env
    val default : int64
    val switch : switch
    val desc : desc
  end
module Int64(I : INT64_VAR) : INT64

module type FLT_VAR = 
  sig
    val env : env
    val default : float
    val switch : switch
    val desc : desc
  end
module Flt(F : FLT_VAR) : FLT

module type BOOL_VAR = 
  sig
    val env : env
    val default : bool
    val switch : switch
    val desc : desc
  end
module Bool(B : BOOL_VAR) : BOOL

module type BYTES_VAR =
    sig
        val env : env
        val default : bytes
        val switch : switch
        val desc : desc
    end
module Bytes(B : BYTES_VAR) : BYTES

module type INET_VAR =
  sig
    val env : env
    val default : inet_addr
    val switch : switch
    val desc : desc
  end
module Inet(I : INET_VAR) : INET

module type INET_DEFAULT =
  sig
    val env : env
    val switch : switch
    val desc : desc
  end
module InetDefault(I : INET_DEFAULT) : INET
module InetAny(I : INET_DEFAULT) : INET

module type FLAG_VAR = BVAR
module Set(F : FLAG_VAR) : BOOL
module Clear(F : FLAG_VAR) : BOOL

module type HOST_VAR =
    sig
        val env : env
        val default : host
        val switch : switch
        val desc : desc
    end
module Host(H : HOST_VAR) : HOST

module type PORT_VAR = 
    sig
        val env : env
        val default : port
        val switch : switch
        val desc : desc
    end
module Port(P : PORT_VAR) : PORT

module type EMAIL_VAR =
    sig
        val env : env
        val default : email
        val switch : switch
        val desc : desc
    end
module Email(E : EMAIL_VAR) : EMAIL

val parse_line : line -> unit
val preprocess : file -> cmd
val parse_file : file -> unit

module Option(C : Ser.ELT)(E : VAR with type elt = C.elt option) :
  ELT with type elt = E.elt

module OptNone(C : Ser.ELT)(E : BVAR) :
    ELT with type elt = C.elt option

module StrOpt(E : VAR with type elt = string option) : 
  ELT with type elt = E.elt

module type SEP = Ser.SEP
module MakeList(Sep : SEP)(Ser : Ser.ELT)(V : VAR with type elt = Ser.elt list) : 
  ELT with type elt = Ser.elt list
module CommaList(C : Ser.ELT)(V : VAR with type elt = C.elt list) :
    ELT with type elt = V.elt
module SpaceList(C : Ser.ELT)(V : VAR with type elt = C.elt list) :
    ELT with type elt = V.elt
module ColonList(C : Ser.ELT)(V : VAR with type elt = C.elt list) :
    ELT with type elt = V.elt

module type STRLIST_VAR =
    sig
        val env : env
        val default : string list
        val switch : switch
        val desc : desc
    end
module type STRLIST = ELT with type elt = string list
module CommaStrList(S : STRLIST_VAR) : STRLIST
module SpaceStrList(S : STRLIST_VAR) : STRLIST
module ColonStrList(S : STRLIST_VAR) : STRLIST

module type TUPLE =
    sig
        module Ser : Ser.ELT 
        module type VAR =
            sig
                val env : env
                val default : Ser.elt
                val switch : switch
                val desc : desc
            end
        module Cfg(V : VAR) : ELT with type elt = Ser.elt
    end
module Tuple(Sep : Ser.SEP)(P1 : Ser.ELT)(P2 : Ser.ELT) :
    TUPLE with type Ser.elt = P1.elt * P2.elt

module ColonTuple(P1 : Ser.ELT)(P2 : Ser.ELT) : 
    TUPLE with type Ser.elt = P1.elt * P2.elt

module type EMAILS_VAR =
    sig
        val env : env
        val default : email list
        val switch : switch
        val desc : desc
    end
module Emails(E : EMAILS_VAR) : EMAILS

module type FILE_VAR = 
    sig
        val env : env
        val default : file
        val switch : switch
        val desc : desc
    end
module type DIR_VAR =
    sig
        val env : env
        val default : dir
        val switch : switch
        val desc : desc
    end

module type FILE = 
    sig
        include ELT with type elt = file
        val env : env
        val file : unit -> elt
        val exists : unit -> bool
    end
module type FILE_OUT = 
    sig
        include FILE
        val is_stdout : unit -> bool
        val use : ?app:bool -> (out_channel -> 'a) -> 'a
    end
module type FILE_IN = 
    sig
        include FILE
        val is_stdin : unit -> bool
        val use : (in_channel -> 'a) -> 'a
    end
module type FILE_FIND =
    sig
        include FILE
        val find : unit -> elt option
    end
module type DIR = 
    sig
        include ELT with type elt = dir
        val dir : unit -> elt
        module File(F : FILE_VAR) : FILE
    end
module type FILE_DIR =
    sig
        include DIR
        module Dir(D : DIR_VAR) : DIR
    end

module File(F : FILE_VAR) : FILE
module Dir(F : FILE_VAR) : DIR
module FileDir(D : DIR_VAR) : FILE_DIR
module SubDir(D : FILE_DIR)(Sub : DIR_VAR) : FILE_DIR

module FileOut(F : BVAR) : FILE_OUT 
module FileIn(F : BVAR) : FILE_IN
module FileFind(F : FILE_VAR) : FILE

module type SADDR_VAR = 
    sig
        val env : env
        val default : Unix.sockaddr
        val switch : switch
        val desc : desc
    end
module SockAddr(SA : SADDR_VAR) : SADDR
module SockAddrAny(SA : BVAR) : SADDR

module type HOSTNAME_VAR = 
    sig
        val env : env
        val switch : switch
        val desc : desc
    end
module Hostname(H : HOSTNAME_VAR) : STR

module type CFG_FILE_VAR =
    sig
        val env : env
    end

module ConfigFile(F : CFG_FILE_VAR) : FILE
module HomeFile(V : BVAR) : FILE_FIND

module type RUN =
    sig
        val run : in_channel -> out_channel -> 'a
    end
module type MAIN_IO = 
    sig
        val run : (in_channel -> out_channel -> 'a) -> 'a
    end

module MainIn(In : FILE) :
    sig
        val run : (in_channel -> 'a) -> 'a
    end
module MainOut(Out : FILE) :
    sig
        val run : (out_channel -> 'a) -> 'a
    end
module MainIO(In : FILE)(Out : FILE) : MAIN_IO
module Main(In : FILE)(Out : FILE) :
    sig
        module IO : MAIN_IO
        module Main(Run : RUN) :
            sig
                val run : unit -> 'a
            end 
        val fold : (out_channel -> line -> 'a -> 'a) -> 'a -> 'a
        val iter : (out_channel -> line -> unit) -> unit
        val map : (out_channel -> line -> 'a) -> 'a list
    end

val load_args : (unit -> file) -> unit
val load_args_extras : (unit -> file) -> string list
val load_config : (unit -> file) -> string list

