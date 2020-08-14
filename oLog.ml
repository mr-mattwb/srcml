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

module Fmt : FMT =
    struct
        let fmt sev msg =
            let tm = Unix.localtime (Unix.time()) in
            sprintf "%04d-%02d-%02d %02d:%02d:%02d % 8d % 4d %s"
                (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
                tm.tm_hour tm.tm_min tm.tm_sec
                (Unix.getpid()) sev msg
    end

module IniLevel : LEVEL = 
    struct
        let level = OIni.int_0 (
            object
                method context = "LOG"
                method env = "level"
                method switch = "--log-level"
                method desc = "Log level.  Msgs below level get logged."
            end)
    end
module IniFile : FILE =
    struct
        let file = OIni.file (
            object
                method context = "LOG"
                method env = "file"
                method switch = "--log-file"
                method desc = "Write Log entries to a file."
                method default = 
                    let bn = Filename.basename Sys.argv.(0) in
                    try (Filename.chop_extension bn) ^ ".log"
                    with Invalid_argument _ -> bn^".log"
            end)
    end
module IniSockAddr : SOCKADDR =
    struct
        let addr = OIni.sockaddr (
            object
                method context = "LOG"
                method env = "sockaddr"
                method switch = "--log-addr"
                method desc = "Socket address"
                method default = ADDR_UNIX ""
            end)
    end

module WriterLevel(Level : LEVEL)(W : WRITER) : WRITER = 
    struct
        let write sev msg = 
            if sev <= Level.level#get then W.write sev msg
            else ()
    end
module WriterCons(A : WRITER)(B : WRITER) : WRITER = 
    struct
        let write sev msg = A.write sev msg; B.write sev msg
    end

module WriterChannel(Fmt : FMT)(Ch : CHANNEL) : WRITER =
    struct
        let write sev msg = 
            output_string Ch.outch (Fmt.fmt sev msg);
            output_char Ch.outch '\n';
            flush Ch.outch
    end
module WriterFile(Fmt : FMT)(F : FILE) : WRITER =
    struct
        let rec append_line line = 
            Utils.file_out_app (output line) F.file#get
        and output line fout = 
            output_string fout line;
            output_char fout '\n';
            flush fout
        let write sev msg = append_line (Fmt.fmt sev msg)

    end
module WriterSockAddr(Fmt : FMT)(N : SOCKADDR) : WRITER = 
    struct
        let net_write msg = 
            let fin, fout = open_connection N.addr#get in
            try
                output_string fout msg;
                output_char fout '\n';
                flush fout;
                close_out fout
            with e ->
                (try close_in fin with _ -> ());
                raise e
        let write sev msg = net_write (Fmt.fmt sev msg)
    end

module WriterStdout(Fmt : FMT) : WRITER = WriterChannel(Fmt)(
    struct
        let outch = Pervasives.stdout
    end)
module WriterStderr(Fmt : FMT) : WRITER = WriterChannel(Fmt)(
    struct
        let outch = Pervasives.stderr
    end)

module LogStdout = WriterStdout(Fmt)
module LogStderr = WriterStderr(Fmt)
module LogFile = WriterFile(Fmt)(IniFile)
module LogSockAddr = WriterSockAddr(Fmt)(IniSockAddr)

module Stdout = WriterLevel(IniLevel)(LogStdout)
module Stderr = WriterLevel(IniLevel)(LogStderr)
module File = WriterLevel(IniLevel)(LogFile)
module SockAddr = WriterLevel(IniLevel)(LogSockAddr)

module Format(W : WRITER) : FORMAT = 
    struct
        let dprintf sev fmt = ksprintf (W.write sev) fmt
    end

type target = 
    | Disabled
    | Stdout
    | Stderr 
    | File of Utils.file
    | Addr of Unix.sockaddr

open OIni

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

module Make(Target : TARGET) =
    struct
        include Target
        module type LOG = 
            sig
                val target : elt OIni.t
            end
        module Writer(Fmt : FMT)(T : LOG) = 
            struct
                let rec write sev rsp = Target.write_log (Fmt.fmt sev rsp) T.target#get
            end
        module OLog =
            struct
                let target = ini cvar
            end
        module WriterFmt(T : LOG) = Writer(Fmt)(T)
        module Logger(L : LEVEL)(T : LOG) = Format(WriterLevel(L)(WriterFmt(T)))
        module Log = Logger(IniLevel)(OLog)
    end

module Target = Make(
    struct
        type elt = target
        let ser : elt OIni.Ser.t = 
            object (self)
                val file = Ser.file
                val addr = Ser.sockaddr
                val sep = Ser.colon_sep
                method to_str = function
                    | Disabled -> "DISABLED"
                    | Stdout -> "STDOUT"
                    | Stderr -> "STDERR"
                    | File f -> "FILE"^sep#sep^(file#to_str f)
                    | Addr sa -> "ADDR"^sep#sep^(addr#to_str sa)
                method of_str s = 
                    match sep#split s with
                    | []  -> Disabled
                    | tag :: rest -> self#tag_of_str tag rest 
                method private tag_of_str tag rest = 
                    match String.uppercase_ascii tag with
                    | "DISABLED"
                    | "STDOUT" -> Stdout
                    | "STDERR" -> Stderr
                    | "FILE"   -> File (self#get_filename rest)
                    | "ADDR"   -> Addr (self#get_addr rest)
                    | ntag     -> 
                        eprintf "Unknown log target [%s]\n%!" tag;
                        raise (Conversion_failure "OLog.target_ser") 
                method private get_filename = function
                    | [] -> raise (Conversion_failure "OLog.target_ser: no file name")
                    | f :: _ -> file#of_str f
                method private get_addr = function
                    | [] -> raise (Conversion_failure "OLog.target_ser: no sockaddr")
                    | sa :: _ -> addr#of_str sa
            end
        let var (c : CVar.t) : elt OIni.OVar.t = OVar.default c Disabled

        class ini_elt (cvar : CVar.t) = 
            object
                inherit [elt] OIni.elt ser (var cvar)
            end
        let ini (cvar : CVar.t) = new ini_elt cvar
        let cvar = 
            object
                method context = "LOG"
                method env = "target"
                method switch = "--target"
                method desc = "Log target destination"
            end

        let rec write_log msg = function
            | Disabled -> ()
            | Stdout -> write_chan msg Pervasives.stdout
            | Stderr -> write_chan msg Pervasives.stderr
            | File f -> Utils.file_out_app (write_chan msg) f
            | Addr sa -> write_addr msg sa
        and write_chan msg fout = 
            output_string fout msg;
            output_char fout '\n';
            flush fout
        and write_addr msg sa = 
            let fin, fout = open_connection sa in
            try
                write_chan msg fout;
                close_in fin
            with e ->
                (try close_in fin with _ -> ());
                raise e
    end)
        

module Targets = Make(
    struct
        type elt = Target.elt list

        let ser = Ser.comma_list Target.ser
        let var cvar = OVar.default cvar []
        let ini cvar = OIni.list_null ser cvar

        let cvar = 
            object
                method context = "LOG"
                method env = "target_list"
                method switch = "--targets"
                method desc = "List of log targets"
            end

        let write_log msg ls = List.iter (Target.write_log msg) ls
    end)

(*
        module type LOG =
            sig
                val target : elt OIni.t
            end
        module Writer(Fmt : FMT)(T : LOG) = 
            struct
                let write sev rsp = 
                    let msg = Fmt.fmt sev rsp in

                    List.iter (Target.write_log msg) T.target#get
            end
        module OLog = struct let target = ini cvar end
        module WriterFmt(T : LOG) = Writer(Fmt)(T)
        module Make(T : LOG) = Format(WriterFmt(T))
        module Log = Make(OLog)
    end
*)

