open Unix
open Printf
open Pervasives

module Rxp = Pcre

exception Conversion of string * string

module type ELT = 
  sig
    type elt
    val of_str : string -> elt
    val to_str : elt -> string
  end
module type STR = ELT with type elt = string
module type INT = ELT with type elt = int
module type INT32 = ELT with type elt = int32
module type INT64 = ELT with type elt = int64
module type FLT = ELT with type elt = float
module type BOOL = ELT with type elt = bool
module type BYTES = ELT with type elt = bytes
module type MSEC = ELT with type elt = Utils.msec

module Str : STR
module Int : INT
module Int32 : INT32
module Int64 : INT64
module Flt : FLT
module Bool : BOOL
module Bytes : BYTES
module MSec : MSEC

module Option(E : ELT) : ELT with type elt = E.elt option

module Inet : ELT with type elt = Unix.inet_addr

val use_dns : string

module type SEP_ARG = 
    sig
        val rxp : Rxp.regexp
        val sep : string
    end

module type SEP =
    sig
        include SEP_ARG
        val split : string -> string list
        val split1 : string -> string * string list
        val split2 : string -> string * string
        val concat : string -> string -> string
        val join : string list -> string
    end

module MakeSep(SA : SEP_ARG) : SEP

module MakeList(Sep : SEP)(C : ELT) : ELT with type elt = C.elt list
module Comma : SEP
module Space : SEP
module Colon : SEP
module SemiColon : SEP
module Bar : SEP
module Equal : SEP
module Dot : SEP
module ColonEq : SEP
module Exclamation : SEP
module Bang : SEP

module CommaList(C : ELT) : ELT with type elt = C.elt list
module SpaceList(C : ELT) : ELT with type elt = C.elt list
module ColonList(C : ELT) : ELT with type elt = C.elt list
module SemiColonList(C : ELT) : ELT with type elt = C.elt list
module BarList(C : ELT) : ELT with type elt = C.elt list

module type STRLIST = ELT with type elt = string list
module CommaStrList : STRLIST
module SpaceStrList : STRLIST
module ColonStrList : STRLIST
module SemiColonStrList : STRLIST
module BarStrList : STRLIST

module Tuple(Sep : SEP)(P1 : ELT)(P2 : ELT) : ELT with type elt = P1.elt * P2.elt
module SpaceTuple(P1 : ELT)(P2 : ELT) : ELT with type elt = P1.elt * P2.elt
module CommaTuple(P1 : ELT)(P2 : ELT) : ELT with type elt = P1.elt * P2.elt
module ColonTuple(P1 : ELT)(P2 : ELT) : ELT with type elt = P1.elt * P2.elt
module EqualTuple(P1 : ELT)(P2 : ELT) : ELT with type elt = P1.elt * P2.elt

module type FILE = ELT with type elt = Utils.file
module type FILELIST = ELT with type elt = Utils.file list
module File : FILE
module FileList : FILELIST

module type DIR = ELT with type elt = Utils.dir
module type DIRLIST = ELT with type elt = Utils.dir list
module Dir : DIR
module DirList : DIRLIST
module DirSet : ELT with type elt = Utils.DirSet.t 

type sock_addr_t = I | U
module SockAddrType : ELT with type elt = sock_addr_t
module UseDNS :
    sig
        include BOOL
        val env : string
        val def : bool
        val get : unit -> bool
        val put : bool -> unit
    end

module type SOCK_ELT = 
    sig
        include ELT with type elt = Unix.sockaddr
        val as_string : elt -> string
        val string_part : elt -> string
    end

module HostInet : ELT with type elt = inet_addr
module AddrInet : ELT with type elt = inet_addr * int
module SockAddrInet : ELT with type elt = inet_addr * int
module MakeSockAddr(E : ELT with type elt = inet_addr * int) : SOCK_ELT

module SocketAddr : SOCK_ELT
module SocketAddress : SOCK_ELT

(* Alternate implementation using Equal(=) to separate I and U from the 
data.  I think this is s the better approach *)
module SockAddr : SOCK_ELT

module Signal : INT

