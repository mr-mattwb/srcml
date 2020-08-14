open Unix
open Printf
open Pervasives

open Utils

type level = int
type message = string

module type MESSAGE = 
    sig
        val message : level -> message -> string
    end
module type WRITER = 
    sig
        val write : level -> message -> unit
    end
module type FORMAT = 
    sig
        val dprintf : level -> ('a, unit, string, unit) format4 -> 'a
    end
module type LEVEL_INI = EnvCfg.ELT with type elt = level
module type LEVEL_VAR = 
    sig
        include EnvCfg.BVAR
        val default : level
    end
    
module type OUTCH = 
    sig
        val outch : out_channel
    end
module type FILE = EnvCfg.FILE
module type ADDR = EnvCfg.SOCKADDR

module Message : MESSAGE

val output_line : out_channel -> message -> unit
val output_line_rev : message -> out_channel -> unit
val output_file : file -> message -> unit
val output_addr : sockaddr -> message -> unit

module MsgChannel(Msg : MESSAGE)(Ch : OUTCH) : WRITER
module MsgFile(Msg : MESSAGE)(File : FILE) : WRITER
module MsgSockAddr(Msg : MESSAGE)(Addr : ADDR) : WRITER

module MsgStdout(Msg : MESSAGE) : WRITER
module MsgStderr(Msg : MESSAGE) : WRITER

module Stdout : WRITER
module Stderr : WRITER
module File(F : FILE) : WRITER
module SockAddr(Addr : ADDR) : WRITER
module Disabled : WRITER
module Cons(W1 : WRITER)(W2 : WRITER) : WRITER

module MessageLevel(L : LEVEL_INI)(M : MESSAGE) : MESSAGE
module MsgLevel(L : LEVEL_INI) : MESSAGE
module Level(L : LEVEL_INI)(W : WRITER) : WRITER

module Format(W : WRITER) : FORMAT
module FmtLevel(L : LEVEL_INI)(W : WRITER) : FORMAT

module Threshold :
    sig
        module Default : EnvCfg.DEFAULT with type elt = level
        module Var : EnvCfg.BVAR 
        module Env(C : EnvCfg.CFG)(V : EnvCfg.BVAR) : LEVEL_INI
        module EnvDefault(C : EnvCfg.CFG)(V : EnvCfg.BVAR) : LEVEL_INI
        module UnixEnv(V : EnvCfg.BVAR) : LEVEL_INI
        module Ini : LEVEL_INI
    end

type target = 
    | Stderr
    | Stdout
    | File of Utils.file
    | Addr of Unix.sockaddr
    | Disabled

module type TARGET_PARAMS =
    sig
        type t
        type elt = t
        val output : message -> elt -> unit
        val default : t
        module Ser : Ser.ELT with type elt = t
        module BVar : EnvCfg.BVAR
        module Var : 
            sig 
                include EnvCfg.BVAR
                val default : elt
            end
    end
module type TARGET_ELT = 
    sig
        type t
        module type VAR = 
            sig
                include EnvCfg.BVAR
                val default : t
            end
        module type SER = Ser.ELT with type elt = t
        module type INI = EnvCfg.ELT with type elt = t
        module Default : EnvCfg.DEFAULT with type elt = t
        module Ser : SER
        module MsgWriter(Msg : MESSAGE)(Ini : INI) : WRITER
        module MsgFmt(Msg : MESSAGE)(Ini : INI) : FORMAT
        module Writer(Ini : INI) : WRITER
        module LevelWriter(L : LEVEL_INI)(Ini : INI) : WRITER
        module Fmt(Ini : INI) : FORMAT
        module LevelFmt(L : LEVEL_INI)(Ini : INI) : FORMAT

        module EnvDefault(C : EnvCfg.CFG)(V : EnvCfg.BVAR) : INI
        module Env(C : EnvCfg.CFG)(V : VAR) : INI
        module UnixEnv(V : VAR) : INI
        module UnixEnvDefault(V : EnvCfg.BVAR) : INI
        module Ini : INI
        module Log : FORMAT
        val register : unit -> unit
  end
module MakeTarget(P : TARGET_PARAMS) : TARGET_ELT with type t = P.t

module Target : TARGET_ELT with type t = target
module Targets : TARGET_ELT with type t = target list

val register : unit -> unit
include FORMAT
