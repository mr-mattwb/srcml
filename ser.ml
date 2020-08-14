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

module Str =
  struct
    type elt = string
    let of_str s = s
    let to_str s = s
  end
module Int = 
  struct
    type elt = int
    let of_str = function
        | "" -> 0
        | n -> int_of_string n
    let to_str = string_of_int
  end
module Int32 = 
  struct
    type elt = int32
    let of_str = function
        | "" -> 0l
        | n -> Int32.of_string n
    let to_str = Int32.to_string
  end
module Int64 = 
  struct
    type elt = int64
    let of_str = function
        | "" -> 0L
        | n -> Int64.of_string n
    let to_str = Int64.to_string
  end
module Flt = 
  struct
    type elt = float
    let of_str = float_of_string
    let to_str = string_of_float
  end
module Bool =
  struct
    type elt = bool
    let of_str s =
      match s.[0] with
      | 'F' | 'f' | 'n' | 'N' | '0' -> false
      | _ -> true
    let to_str = function
      | false -> "false"
      | true -> "true"
  end
module Bytes = 
    struct
        type elt = bytes
        let of_str = Bytes.of_string
        let to_str = Bytes.to_string
    end

module MSec = Int32

module Option(E : ELT) =
  struct
    type elt = E.elt option
    let of_str = function
      | "" -> None
      | s -> Some (E.of_str s)
    let to_str = function
      | None -> ""
      | Some f -> E.to_str f
  end

module Inet = 
    struct
        type elt = Unix.inet_addr
        let of_str s = Unix.inet_addr_of_string s
        let to_str s = Unix.string_of_inet_addr s
    end

let use_dns = "USE_DNS"

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

module MakeSep(SA : SEP_ARG) = 
    struct
        include SA
        let split str = Rxp.split ~rex:rxp str
        let split1 str = 
            match Rxp.split ~rex:rxp str with
            | [] -> "", []
            | x :: [] -> x, []
            | x :: xs -> x, xs
        let split2 str = 
            match Rxp.split ~rex:rxp ~max:2 str with
            | [] -> "", ""
            | x :: [] -> x, ""
            | x :: arg :: [] -> x, arg
            | _ -> raise (Failure "Pcre.split")
        let concat s1 s2 = s1^sep^s2
        let join ls =
            let buf = Buffer.create 1024 in
            let add x = Buffer.add_string buf x in
            let next x = Buffer.add_string buf sep; add x in
            match ls with
            | [] -> ""
            | x :: [] -> x
            | x :: xs -> add x; List.iter next xs; Buffer.contents buf
    end

module MakeList(Sep : SEP)(C : ELT) = 
    struct
        type elt = C.elt list
        let of_str s = 
            let of_str s = 
                try C.of_str s
                with Failure f -> raise (Failure (s^":"^f))
            in
            List.map of_str (Sep.split s)
        let to_str ls = 
            let buf = Buffer.create 1024 in
            let add_elt b = 
                Buffer.add_string buf (C.to_str b) in
            let accum () b = Buffer.add_string buf Sep.sep; add_elt b in
            match ls with
            | [] -> ""
            | x :: [] -> C.to_str x
            | x :: xs -> List.fold_left accum (add_elt x) xs; Buffer.contents buf

    end

let wspc = "[\r\n\t ]"
let wspcs = wspc^"*"
module Comma = MakeSep(
    struct
        let rxp = Rxp.regexp "[\t\r\n ]*,[\t\r\n ]*"
        let sep = ","
    end)
module Space = MakeSep(
    struct
        let rxp = Rxp.regexp "[\t\r\n ]+"
        let sep = " "
    end)
module Colon = MakeSep(
    struct
        let rxp = Rxp.regexp (wspcs^":"^wspcs)
        let sep = ":"
    end)
module SemiColon = MakeSep(
    struct
        let rxp = Rxp.regexp (wspcs^";"^wspcs)
        let sep = ";"
    end)
module Bar = MakeSep(
    struct
        let rxp = Rxp.regexp (wspcs^(Rxp.quote "|")^wspcs)
        let sep = "|"
    end)
module Equal = MakeSep(
    struct
        let rxp = Rxp.regexp "="
        let sep = "="
    end)
module Dot = MakeSep(
    struct
        let rxp = Rxp.regexp "\\."
        let sep = "."
    end)
module ColonEq = MakeSep(
    struct
        let rxp = Rxp.regexp ":="
        let sep = ":="
    end)
module Exclamation = MakeSep(
    struct
        let rxp = Rxp.regexp "!"
        let sep = "!"
    end)
module Bang = Exclamation

module CommaList = MakeList(Comma)
module SpaceList = MakeList(Space)
module ColonList = MakeList(Colon)
module SemiColonList = MakeList(SemiColon)
module BarList = MakeList(Bar)

module type STRLIST = ELT with type elt = string list
module CommaStrList = CommaList(Str)
module SpaceStrList = SpaceList(Str)
module ColonStrList = ColonList(Str)
module SemiColonStrList = SemiColonList(Str)
module BarStrList = BarList(Str)

module Tuple(Sep : SEP)(P1 : ELT)(P2 : ELT) = 
    struct
        type elt = P1.elt * P2.elt
        let of_str s = 
            let p1, p2 = Sep.split2 s in
            (P1.of_str p1, P2.of_str p2)
        let to_str (p1, p2) = 
            Sep.join [P1.to_str p1; P2.to_str p2]
    end
module SpaceTuple = Tuple(Space)
module CommaTuple = Tuple(Comma)
module ColonTuple = Tuple(Colon)
module EqualTuple = Tuple(Equal)

module type FILE = ELT with type elt = Utils.file
module type FILELIST = ELT with type elt = Utils.file list
module File = Str
module FileList = ColonStrList

module type DIR = ELT with type elt = Utils.dir
module type DIRLIST = ELT with type elt = Utils.dir list
module Dir = Str
module DirList = ColonStrList
module DirSet =
    struct
        open Utils
        type elt = DirSet.t
        let of_str str = 
            let accum set d = DirSet.add d set in
            List.fold_left accum DirSet.empty (DirList.of_str str)
        let to_str set = 
            DirList.to_str (DirSet.elements set)
    end

type sock_addr_t = I | U
module SockAddrType =
    struct
        type elt = sock_addr_t
        let of_str s = 
            match Utils.upper s with
            | "I" -> I
            | "U" -> U
            | _ -> I
        let to_str = function
            | I -> "I"
            | U -> "U"
    end
module UseDNS = 
    struct
        include Bool
        let env = "USE_DNS"
        let def = true
        let get () =
            try of_str (Unix.getenv env)
            with _ -> def
        let put v = 
            Unix.putenv env (try to_str v with _ -> to_str def)
    end

module type SOCK_ELT = 
    sig
        include ELT with type elt = Unix.sockaddr
        val as_string : elt -> string
        val string_part : elt -> string
    end 

module HostInet =
    struct
        type elt = Unix.inet_addr
        let of_str host = 
            if host = "" then inet_addr_any
            else if (UseDNS.get()) then
                try (gethostbyname host).h_addr_list.(0)
                with _ -> Inet.of_str host
            else
                Inet.of_str host
        let to_str addr = 
            if (UseDNS.get()) then
                try (gethostbyaddr addr).h_name
                with _ -> Inet.to_str addr 
            else
                Inet.to_str addr
    end

module type OF_STR_LIST = 
    sig
        val of_str_list : string list -> inet_addr * int
    end
module Addr = HostInet
module Port = Int
let null_inet = (inet_addr_any, 0)

module MakeInetSockAddr(OSL : OF_STR_LIST) = 
    struct
        type elt = inet_addr * int
        module Sep = Space
        let rec of_str = function
            | "" -> null_inet
            | ia -> OSL.of_str_list (Sep.split ia)
        let to_str (ia, p) = (Addr.to_str ia)^Sep.sep^(Port.to_str p)
    end

module AddrInet = MakeInetSockAddr(
    struct
        let of_str_list = function
            | [] -> null_inet
            | ad :: [] -> (Addr.of_str ad, 0)
            | ad :: pt :: _ -> (Addr.of_str ad, Port.of_str pt)
    end)
module SockAddrInet = MakeInetSockAddr(
    struct
        let of_str_list = function
            | ad :: pt :: _ -> (Addr.of_str ad, Port.of_str pt)
            | _ -> raise (Failure "SockAddrInet:  Port is required")
    end)
module MakeSockAddr(E : ELT with type elt = inet_addr * int) =
    struct
        type elt = sockaddr
        let make_inet (ia, p) = ADDR_INET(ia, p)
        let rec of_str = function
            | ""
            | "U" -> ADDR_UNIX ""
            | "I" -> ADDR_INET (inet_addr_any, 0)
            | str -> of_substr str (String.sub str 1 (String.length str - 1)) str.[0]
        and of_substr orig subs = function
            | 'U' -> ADDR_UNIX subs
            | 'I' -> make_inet (E.of_str subs)
            | _ -> unknown orig
        and unknown str = 
            try make_inet (E.of_str str)
            with _ -> ADDR_UNIX str
        let to_str = function
            | ADDR_UNIX "" -> "U"
            | ADDR_INET (ia, 0) when ia = inet_addr_any -> "I"
            | ADDR_UNIX s -> "U"^s
            | ADDR_INET (ia, p) -> "I"^(E.to_str (ia, p))

        let as_string = function
            | ADDR_UNIX fname -> fname
            | ADDR_INET (ia, p) -> E.to_str (ia, p)
        let string_part = function
            | ADDR_UNIX fname -> fname
            | ADDR_INET (ia, p) -> Addr.to_str ia
    end
module SocketAddr = MakeSockAddr(SockAddrInet)
module SocketAddress = MakeSockAddr(AddrInet)
module SockAddr =
    struct
        type sock_t = I | U
        module SockT = 
            struct
                type elt = sock_t
                let of_str s = 
                    match Utils.upper s with
                    | "U" -> U
                    | _ -> I
                and to_str = function
                    | U -> "U"
                    | I -> "I"
            end
        module InetAddr = ColonTuple(Addr)(Port)
        module SockA = SpaceTuple(SockT)(Str)
        type elt = sockaddr
        let of_str s = 
            match SockA.of_str s with
            | (I, iap) -> 
                let ia, p = InetAddr.of_str iap in
                ADDR_INET(ia, p)
            | (U, path) ->
                ADDR_UNIX path
        let to_str = function
            | ADDR_INET (ia, p) -> SockA.to_str (I, InetAddr.to_str (ia, p))
            | ADDR_UNIX path -> SockA.to_str (U, path)

        let as_string = function
            | ADDR_INET (ia, p) -> InetAddr.to_str (ia, p)
            | ADDR_UNIX path -> path
        let string_part = function
            | ADDR_INET (ia, p) -> InetAddr.to_str (ia, p)
            | ADDR_UNIX path -> path
    end

module Signal = 
    struct
        type elt = int
        let sigmap = [
            "SIGABRT", Sys.sigabrt; "SIGALRM", Sys.sigalrm;
            "SIGBUS", Sys.sigbus;  "SIGCHLD", Sys.sigchld;
            "SIGCONT", Sys.sigcont; "SIGFPE",  Sys.sigfpe;
            "SIGHUP", Sys.sighup;  "SIGILL", Sys.sigill;
            "SIGINT", Sys.sigint;  "SIGKILL", Sys.sigkill;
            "SIGPIPE", Sys.sigpipe; "SIGPOLL", Sys.sigpoll;
            "SIGPROF", Sys.sigprof; "SIGQUIT", Sys.sigquit;
            "SIGSEGV", Sys.sigsegv; "SIGSTOP", Sys.sigstop;
            "SIGSYS", Sys.sigsys;  "SIGTERM", Sys.sigterm;
            "SIGTRAP", Sys.sigtrap; "SIGTSTP", Sys.sigtstp;
            "SIGTTIN", Sys.sigttin; "SIGTTOU", Sys.sigttou;
            "SIGURG", Sys.sigurg;  "SIGUSR1", Sys.sigusr1;
            "SIGUSR2", Sys.sigusr2; "SIGVTALRM", Sys.sigvtalrm;
            "SIGXCPU", Sys.sigxcpu; "SIGXFSZ", Sys.sigxfsz]

        let to_int =
            let tbl = Hashtbl.create 28 in
            let add (st, si) = Hashtbl.add tbl st si in
            List.iter add sigmap;
            tbl
        let of_int = 
            let tbl = Hashtbl.create 28 in
            let add (st, si) = Hashtbl.add tbl si st in
            List.iter add sigmap;
            tbl
        let of_str s = 
            match Hashtbl.find_opt to_int (String.uppercase_ascii s) with
            | None -> raise (Failure ("Uknown signal:"^s))
            | Some si -> si
        let to_str si = 
            match Hashtbl.find_opt of_int si with
            | None -> raise (Failure ("Unknown signal:"^(string_of_int si)))
            | Some st -> st
    end

