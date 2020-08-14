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

module Message : MESSAGE = 
    struct
        let message level msg = 
            let tm = Unix.localtime (Unix.time()) in
            sprintf "%04d-%02d-%02d %02d:%02d:%02d % 8d % 4d %s"
                (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
                tm.tm_hour tm.tm_min tm.tm_sec
                (Unix.getpid()) level msg
    end

let output_line fout msg = 
    try
        Pervasives.output_string fout msg;
        Pervasives.output_char fout '\n';
        flush fout
    with _ -> 
        ()
let output_line_rev msg fout = output_line fout msg
let output_file f msg = Utils.file_out_app (output_line_rev msg) f
let output_addr sa msg = 
    let fin, fout = open_connection sa in
    try
        output_line fout msg;
        close_in fin
    with e ->
        (try close_in fin with _ -> ());
        raise e

module MsgChannel(Msg : MESSAGE)(Ch : OUTCH) : WRITER = 
    struct
        open Pervasives
        let write level msg = 
            try output_line Ch.outch (Msg.message level msg)
            with Sys_error _ -> ()
    end
module MsgFile(Msg : MESSAGE)(File : FILE) : WRITER = 
    struct
        open Pervasives
        let output level msg fout = output_line fout (Msg.message level msg)
        let write level msg = 
            Utils.file_out_app (output level msg) (File.file())
    end
module MsgSockAddr(Msg : MESSAGE)(Addr : ADDR) : WRITER = 
    struct
        let write level msg = output_addr (Addr.get()) (Msg.message level msg)
    end

module MsgStdout(Msg : MESSAGE) = MsgChannel(Msg)(
    struct
        let outch = Pervasives.stdout
    end)
module MsgStderr(Msg : MESSAGE) = MsgChannel(Msg)(
    struct
        let outch = Pervasives.stderr
    end)

module Stdout = MsgStdout(Message)
module Stderr = MsgStderr(Message)
module File(F : FILE) = MsgFile(Message)(F)
module SockAddr(Addr : ADDR) = MsgSockAddr(Message)(Addr)
module Disabled = 
    struct
        let write level msg = ()
    end
module Cons(W1 : WRITER)(W2 : WRITER) : WRITER = 
    struct
        let write level msg = 
            W1.write level msg;
            W2.write level msg
    end

module MessageLevel(L : LEVEL_INI)(M : MESSAGE) =
    struct
        let message level msg = 
            if level <= (L.get()) then msg
            else ""
    end
module MsgLevel(L : LEVEL_INI) = MessageLevel(L)(Message)
module Level(L : LEVEL_INI)(W : WRITER) : WRITER =
    struct
        let write level msg = 
            if level <= (L.get()) then W.write level msg
            else ()
    end

module Format(W : WRITER) = 
    struct
        let dprintf level fmt = ksprintf (W.write level) fmt
    end
module FmtLevel(L : LEVEL_INI)(W : WRITER) = 
    struct
        let dprintf level fmt = 
            let write msg = 
                if level <= (L.get()) then W.write level msg
                else ()
            in
            ksprintf write fmt
    end

module Threshold = 
    struct
        module Default = 
            struct
                type elt = level
                let default = 0
            end
        module Var = 
            struct
                let context = "LOG"
                let env = "threshold"
                let switch = "--log-threshold"
                let desc = "Log threshold.  Messages below threshold get logged."
            end
        module Env(C : EnvCfg.CFG)(V : EnvCfg.BVAR) = C.Int(
            struct
                include Var
                let default = 0
            end)
        module EnvDefault(C : EnvCfg.CFG)(V : EnvCfg.BVAR) = C.MakeDefault(Ser.Int)(Default)(V)
        module UnixEnv = Env(EnvUnix)
        module Ini = UnixEnv(Var)
    end

type target = 
    | Stderr
    | Stdout
    | File of Utils.file
    | Addr of Unix.sockaddr
    | Disabled

let output_target msg = function
    | Disabled -> ()
    | Stderr -> output_line Pervasives.stderr msg
    | Stdout -> output_line Pervasives.stdout msg
    | File f -> output_file f msg
    | Addr a -> output_addr a msg

let output_list msg ls = List.iter (output_target msg) ls

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
        module type INI = EnvCfg.ELT with type elt = t
        module type SER = Ser.ELT with type elt = t
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

module MakeTarget(P : TARGET_PARAMS) : TARGET_ELT with type t = P.t = 
    struct
        type t = P.t
        module type VAR = 
            sig
                include EnvCfg.BVAR
                val default : t
            end
        module type SER = Ser.ELT with type elt = P.elt
        module type INI = EnvCfg.ELT with type elt = P.elt
        module Default =
            struct
                type elt = t
                let default = P.default
            end
        module Ser = P.Ser
        module MsgWriter(Msg : MESSAGE)(Ini : INI) : WRITER = 
            struct
                let write sev msg = P.output (Msg.message sev msg) (Ini.get())
            end
        module MsgFmt(Msg : MESSAGE)(Ini : INI) : FORMAT =
            struct
                let dprintf sev fmt = 
                    let output msg = P.output (Msg.message sev msg) (Ini.get()) in
                    ksprintf output fmt
            end
        module Writer(Ini : INI) : WRITER = MsgWriter(Message)(Ini)
        module LevelWriter(L : LEVEL_INI)(Ini : INI) = Level(L)(Writer(Ini))
        module Fmt(Ini : INI) : FORMAT = MsgFmt(Message)(Ini)
        module LevelFmt(L : LEVEL_INI)(Ini : INI) : FORMAT = Format(LevelWriter(L)(Ini))

        module EnvDefault(C : EnvCfg.CFG)(V : EnvCfg.BVAR) = C.MakeDefault(Ser)(Default)(V)
        module Env(C : EnvCfg.CFG)(V : VAR) = C.Make(Ser)(
            struct
                type elt = t
                include V
            end)
        module UnixEnv(V : VAR) : INI = Env(EnvUnix)(V)
        module UnixEnvDefault(V : EnvCfg.BVAR) = EnvDefault(EnvUnix)(V)
        module Ini : INI = UnixEnv(P.Var)
        module Threshold = Threshold.Ini
        module Log = LevelFmt(Threshold)(Ini)
        let register () = 
            Threshold.register();
            Ini.register()
    end

module Target : TARGET_ELT with type t = target = MakeTarget(
    struct
        type t = target
        type elt = t
        let output = output_target
        let default = Disabled
        module Ser = 
            struct
                module Sep = Ser.Equal
                module SAddr = Ser.SockAddr
                type elt = target
                let rec of_str str = 
                    match Sep.split str with
                    | [] -> Disabled
                    | cmd :: [] -> of_str_cmd cmd
                    | cmd :: arg :: _ -> of_str_cmd_arg cmd arg 
                and of_str_cmd cmd = 
                    match String.uppercase_ascii cmd with
                    | "DISABLED"    -> Disabled
                    | "STDERR"      -> Stderr
                    | "STDOUT"      -> Stdout
                    | _             -> Disabled
                and of_str_cmd_arg cmd arg = 
                    match String.uppercase_ascii cmd with
                    | "FILE"        -> File arg
                    | "ADDR"        -> Addr (SAddr.of_str arg)
                    | _             -> Disabled
                let to_str = function
                    | Disabled      -> "DISABLED"
                    | Stderr        -> "STDERR"
                    | Stdout        -> "STDOUT"
                    | File fn       -> "FILE"^Sep.sep^fn
                    | Addr a        -> "ADDR"^Sep.sep^(SAddr.to_str a)
            end
        module BVar = 
            struct
                let context = "LOG"
                let env = "target"
                let switch = "--log-target"
                let desc = "A log destination:  disabled,stdout,stderr,file f,addr a"
            end
        module Var = 
            struct
                include BVar
                let default = default
            end
    end)
module Targets : TARGET_ELT with type t = target list = MakeTarget(
    struct
        type t = target list
        type elt = t
        let output = output_list
        let default = [Disabled]
        module Ser = Ser.CommaList(Target.Ser)
        module BVar = 
            struct
                let context = "LOG"
                let env = "targets"
                let switch = "--log-targets"
                let desc = "A comma-separated list of log destinations."
            end
        module Var = 
            struct
                include BVar
                let default = default
            end
    end)

let register = Targets.register
include Targets.Log
