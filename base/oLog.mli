open Unix
open Printf
open Pervasives

open OIni

type severity = int
type message = string

module type FMT = 
    sig
        val fmt : severity -> message -> string
    end
module type WRITER = 
    sig
        val write : severity -> message -> unit
    end
module type CHANNEL = 
    sig
        val outch : out_channel
    end
module type FILE = 
    sig
        val file : OIni.file_t
    end
module type SOCKADDR =
    sig
        val addr : sockaddr OIni.t
    end
module type LEVEL = 
    sig
        val level : severity OIni.t
    end
module type FORMAT = 
    sig
        val dprintf : severity -> ('a, unit, string, unit) format4 -> 'a
    end

module Fmt : FMT

(* Ini variables *)
module IniLevel : LEVEL
module IniFile : FILE
module IniSockAddr : SOCKADDR

(* Wrap the writer with a Level module *)
module WriterLevel(Level : LEVEL)(W : WRITER) : WRITER

(* Compose multiple writers *)
module WriterCons(A : WRITER)(B : WRITER) : WRITER

(* Formatters *)
module WriterChannel(Fmt : FMT)(Ch : CHANNEL) : WRITER
module WriterFile(Fmt : FMT)(F : FILE) : WRITER
module WriterSockAddr(Fmt : FMT)(N : SOCKADDR) : WRITER

(* Channels without formatters *)
module WriterStderr(Fmt : FMT) : WRITER
module WriterStdout(Fmt : FMT) : WRITER

(* Writers *)
module LogStdout : WRITER
module LogStderr : WRITER
module LogFile : WRITER
module LogSockAddr : WRITER

(* Level-wrapped writers *)
module Stdout : WRITER
module Stderr : WRITER
module File : WRITER
module SockAddr : WRITER

(* printf-style logging *)
module Format(W : WRITER) : FORMAT

open OIni

type target = 
    | Disabled
    | Stdout
    | Stderr 
    | File of Utils.file
    | Addr of Unix.sockaddr

module type TARGET =
    sig
        type elt
        val ser : elt Ser.t 
        val var : CVar.t -> elt OVar.t
        val ini : CVar.t -> elt OIni.t
        val cvar : CVar.t
        val write_log : message -> elt -> unit
    end
module type ELT =
    sig
        include TARGET
        module type LOG = 
            sig
                val target : elt OIni.t
            end
        module Writer(Fmt : FMT)(T : LOG) : WRITER
        module OLog : LOG
        module WriterFmt(T : LOG) : WRITER
        module Logger(L : LEVEL)(T : LOG) : FORMAT
        module Log : FORMAT
    end

module Target : ELT
module Targets : ELT

