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

module Load(Env : ENV)(Params : PARAMS) : LOAD = 
    struct
        let env_parse = Env.parse

        let rec load get = 
            let _ = load_params () in
            load_file (get());
            load_params ()

        and load_option get = 
            let _ = load_params () in
            load_file_option (get());
            load_params ()

        and load_all get = 
            let _ = load_params () in
            List.iter load_file (get());
            load_params ()

        and load_params () =
            let rest = Params.parse_params () in
            Arg.current := 0;
            rest

        and load_file = function
            | "" -> ()
            | fn when not (Sys.file_exists fn) -> ()
            | fn -> Env.parse fn

        and load_file_option = function
            | None -> ()
            | Some fn -> load_file fn

        and load_files ls = List.iter load_file ls
    end

module Params : PARAMS = 
    struct
        module Specs = Set.Make(
            struct
                type t = spec
                let compare (a, _, _) (b, _, _) = String.compare a b
            end)
        let gSpecs = ref Specs.empty

        let register ss = 
            if Specs.mem ss !gSpecs then ()
            else gSpecs := Specs.add ss !gSpecs

        let env_invalid_arg = "INV_ARG_MSG"
        let def_invalid_arg = "Invalid argument"
        let invalid_arg = 
            try Unix.getenv env_invalid_arg
            with Not_found -> def_invalid_arg

        let param_list () = Specs.elements !gSpecs
        let parse_cli_params () =
            let extras = ref [] in
            let add v = extras := v :: !extras in
            Arg.parse (param_list()) add invalid_arg;
            List.rev !extras
        let parse_params () =
            if !Sys.interactive then []
            else parse_cli_params ()
    end

let make_desc ctx env sw ds = sprintf "[%s][%s][%s] %s" ctx env sw ds

module Make(Env : ENV)(Params : PARAMS) = 
    struct
        exception No_default of string
        let ctx = Env.default_ctx
        module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) = 
            struct
                include V
                let rec get () =
                    match Env.get ~ctx:context env with
                    | None -> default
                    | Some s -> S.of_str s
                and put v = 
                    Env.put ~ctx:context env (S.to_str v)
                let desc = make_desc context env switch desc
                let set_str s = 
                    Env.put ~ctx:context env s 
                let arg = (switch, Arg.String set_str, desc)
                let register () = 
                    if switch <> "" then Params.register arg
                    else ()
                let _ = Env.update context env (S.to_str default)
                let of_str s = put (S.of_str s)
                let to_str () = S.to_str (get())
                let name () = context_name context env
            end
        module MakeDefault(S : Ser.ELT)(D : DEFAULT with type elt = S.elt)(V : BVAR) = 
            struct
                include D
                include V
                let rec get () = 
                    match Env.get ~ctx:context env with
                    | None -> D.default
                    | Some s -> S.of_str s
                and put v = 
                    Env.put ~ctx:context env (S.to_str v)
                let desc = make_desc context env switch desc
                let set_str s = 
                    Env.put ~ctx:context env s
                let arg = (switch, Arg.String set_str, desc)
                let register () = 
                    if switch <> "" then Params.register arg
                    else ()
                let _ = Env.update context env (S.to_str D.default)
                let of_str s = put (S.of_str s)
                let to_str () = S.to_str (get())
                let name () = context_name context env
            end
        module Hide(Elt : ELT) = 
            struct
                include Elt
                let register () = ()
            end
        module Signal(V : VAR_STR) = Make(Ser.Signal)(
            struct
                type elt = int
                include V
                let default = Ser.Signal.of_str V.default
            end)
        module ISignal(V : VAR_INT) = Signal(
            struct
                include V
                let default = Ser.Signal.to_str V.default
            end)
        module Str(V : VAR_STR) = Make(Ser.Str)(struct type elt = string include V end)
        module StrEmpty(V : BVAR) = Str(struct let default = "" include V end)
        module Int(V : VAR_INT) = Make(Ser.Int)(struct type elt = int include V end)
        module Int0(V : BVAR) = Int(struct let default = 0 include V end)
        module Int32(V : VAR_INT32) = Make(Ser.Int32)(struct type elt = int32 include V end)
        module Int64(V : VAR_INT64) = Make(Ser.Int64)(struct type elt = int64 include V end)
        module Flt(V : VAR_FLT) = Make(Ser.Flt)(struct type elt = float include V end)
        module Flt0(V : BVAR) = Flt(struct type elt = float let default = 0.0 include V end)
        module Ser_Char =
            struct
                type elt = char
                let of_str = function
                    | "" -> '\000'
                    | n -> n.[0]
                let to_str ch = String.make 1 ch
            end
        module Char(V : VAR_CHAR) = Make(Ser_Char)(struct type elt = char include V end)
        module Byte(V : VAR_BYTES) = Make(Ser.Bytes)(struct type elt = bytes include V end)
        module Bool(V : VAR_BOOL) = Make(Ser.Bool)(struct type elt = bool include V end)
        module Flag(V : VAR_FLAG)(D : sig val default : bool end) = 
            struct
                include V
                include D
                module Cvt = Ser.Bool
                type elt = bool
                let rec get () =
                    match Env.get ~ctx:context env with
                    | None -> put default; get ()
                    | Some v -> Cvt.of_str v
                and put v = Env.put ~ctx:context env (Cvt.to_str v)
                and arg = 
                    let md = make_desc context env switch desc in
                    (V.switch, Arg.Unit (fun () -> put (not default)), md)
                let register () = 
                    if switch <> "" then Params.register arg
                    else ()
                let _ = Env.update context env (Cvt.to_str default)
                let to_str () = Cvt.to_str (get())
                let of_str s = put (Cvt.of_str s)
                let name () = context_name context env
            end
        module Set(V : VAR_FLAG) = Flag(V)(struct let default = false end)
        module Clear(V : VAR_FLAG) = Flag(V)(struct let default = true end)
        module Dir(V : VAR_DIR) = 
            struct
                include Str(V)
                let rec dir () = get ()
                and dir_ex () = 
                    if exists () then Some (dir())
                    else None
                and exists () = 
                    let d = dir () in
                    Sys.file_exists d && Sys.is_directory d
                let make perms = 
                    match dir_ex () with
                    | None -> Unix.mkdir (get()) perms; dir_ex ()
                    | Some d -> Some d
            end
        module File(V : VAR_FILE) = 
            struct
                include Str(V)
                let rec file () = get ()
                and file_ex () = 
                    if exists () then Some (file())
                    else None
                and exists () = Sys.file_exists (file())
                let touch () = 
                    let ign _ = () in
                    Utils.file_out_app ign (file());
                    file_ex()
            end
        module Sec(V : VAR_SEC) = Int(V)
        module MSec(V : VAR_MSEC) = Int32(V)

        module InetAddr(V : VAR_INET_ADDR) = Make(Ser.Inet)(
            struct
                type elt = inet_addr
                include V
            end)
        module Hostname(V : VAR_STR) = InetAddr(
            struct
                let context = V.context
                let env = V.env
                let switch = V.switch
                let desc = V.desc
                let default = Ser.HostInet.of_str V.default
            end)
        module SockAddr(V : VAR_SOCKADDR) = 
            struct 
                include Make(Ser.SockAddr)( struct type elt = sockaddr include V end)
                let as_string () = Ser.SockAddr.as_string (get())
                let string_part () = Ser.SockAddr.string_part (get())
                let sock_type () = 
                    match get() with
                    | ADDR_INET _ -> PF_INET
                    | ADDR_UNIX _ -> PF_UNIX
                let sock_stream () = Unix.socket (sock_type()) SOCK_STREAM 0
            end
        module SockAddress(V : VAR_SOCKADDR) = 
            struct
                include Make(Ser.SocketAddress)(struct type elt = sockaddr include V end)
                let as_string () = Ser.SocketAddress.as_string (get())
                let string_part () = Ser.SocketAddress.string_part (get())
                let sock_type () = 
                    match get() with
                    | ADDR_INET _ -> PF_INET
                    | ADDR_UNIX _ -> PF_UNIX
                let sock_stream ()= Unix.socket (sock_type()) SOCK_STREAM 0
            end
        module SockAny(V : BVAR) = SockAddr(
            struct
                include V
                let default = ADDR_INET(inet_addr_any, 0)
            end)

        module Rex(V : VAR_STR) =
            struct
                include Str(V)
                let rex () = Pcre.regexp (get())
                let split src = Pcre.split ~rex:(rex()) src
                let replace templ src = Pcre.replace ~rex:(rex()) ~templ src
                let matches src = Pcre.pmatch ~rex:(rex()) src
            end

        module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) =
            struct
                include V
                let default_str = 
                    match default with
                    | None -> ""
                    | Some s -> S.to_str s
                let rec get () =
                    match Env.get ~ctx:context env with
                    | None  -> None
                    | Some str -> Some (S.of_str str)
                and put = function
                    | None -> Env.put ~ctx:context env default_str
                    | Some s -> Env.put ~ctx:context env (S.to_str s)
                let arg = 
                    let desc = make_desc context env switch desc in
                    let setter str = Env.put ~ctx:context env str in
                    (switch, Arg.String setter, desc)
                let register () = 
                    if switch <> "" then Params.register arg
                    else ()
                let _ = Env.update context env default_str
                let to_str () = 
                    match get() with
                    | None -> ""
                    | Some s -> S.to_str s
                let of_str = function
                    | "" -> put None
                    | ss -> put (Some (S.of_str ss))
                let name () = context_name context env
            end
        module OptionNone(S : Ser.ELT)(V : BVAR) = Option(S)(
            struct
                type elt = S.elt option
                let default = None
                include V
            end)
        module MakeList(Sep : Ser.SEP)(E : Ser.ELT)(V : VAR with type elt = E.elt list) =
            struct
                module S = Ser.MakeList(Sep)(E)
                include V
                let get () =
                    match Env.get ~ctx:context env with
                    | None -> default
                    | Some str -> S.of_str str
                let put_str s = Env.put ~ctx:context env s
                let put = function
                    | [] -> put_str ""
                    | ls -> put_str (S.to_str ls)
                let arg = 
                    let desc = make_desc context env switch desc in
                    let setter v = Env.put ~ctx:context env v in
                    (V.switch, Arg.String setter, desc)
                let register () = 
                    if switch <> "" then Params.register arg
                    else ()
                let _ = Env.update context env (S.to_str default)
                let of_str s = put (S.of_str s)
                let to_str () = S.to_str (get())
                let name () = context_name context env
            end
        module ListNull(Sep : Ser.SEP)(S : Ser.ELT)(V : BVAR) = MakeList(Sep)(S)(
            struct
                type elt = S.elt list
                let default = []
                include V
            end)
        module CommaList(S : Ser.ELT)(V : VAR with type elt = S.elt list) = MakeList(Ser.Comma)(S)(V)
        module SpaceList(S : Ser.ELT)(V : VAR with type elt = S.elt list) = MakeList(Ser.Space)(S)(V)
        module ColonList(S : Ser.ELT)(V : VAR with type elt = S.elt list) = MakeList(Ser.Colon)(S)(V)
        module SemiColonList(S : Ser.ELT)(V : VAR with type elt = S.elt list) = MakeList(Ser.SemiColon)(S)(V)
        module BarList(S : Ser.ELT)(V : VAR with type elt = S.elt list) = MakeList(Ser.Bar)(S)(V)

        module StrList(Sep : Ser.SEP)(V : VAR_STRLIST) = MakeList(Sep)(Ser.Str)(
            struct
                type elt = string list
                include V
            end)
        module CommaStrList(V : VAR_STRLIST) = StrList(Ser.Comma)(V)
        module SpaceStrList(V : VAR_STRLIST) = StrList(Ser.Space)(V)
        module ColonStrList(V : VAR_STRLIST) = StrList(Ser.Colon)(V)
        module SemiColonStrList(V : VAR_STRLIST) = StrList(Ser.SemiColon)(V)
        module BarStrList(V : VAR_STRLIST) = StrList(Ser.Bar)(V)

        module DirList(V : VAR_DIRLIST) = MakeList(Ser.Colon)(Ser.Dir)(
            struct
                type elt = Utils.dir list
                include V
            end)
        module FileList(V : VAR_FILELIST) = MakeList(Ser.Colon)(Ser.File)(
            struct
                type elt = Utils.file list
                include V
            end)
        module DirSet(V : BVAR) : DIRSET = 
            struct
                include Make(Ser.DirSet)(
                    struct
                        type elt = Utils.DirSet.t
                        include V
                        let default = Utils.DirSet.empty
                    end)
                let to_list () = Utils.DirSet.elements (get())
                let add d = put (Utils.DirSet.add d (get()))
                let add_list ls =
                    let set = get () in
                    let accum set d = Utils.DirSet.add d set in
                    let set' = List.fold_left accum set ls in
                    put set'
            end

        module Debug = 
            struct
                include Set(
                    struct
                        let context = ""
                        let env = "DEBUG"
                        let switch = "--debug"
                        let desc = "Debugging variable"
                    end)
                module LogF = Cfg.File(
                    struct
                        let context = ""
                        let env = "DEBUG_FILE"
                        let default = "" (* (Utils.name()) ^ ".log" *)
                        let switch = "--dbg-file"
                        let desc = "File to log debug messages"
                    end)
                module LogFile = Log.File(struct let file = LogF.file() end)
                module LogCons = Log.Stderr 
                let dprintf fmt = 
                    let writer msg = 
                        if get() then
                            if LogF.file() <> "" then ignore (LogFile.dprintf 0 "%s" msg) 
                            else ignore (LogCons.dprintf 0 "%s" msg)
                    in
                    ksprintf writer fmt
            end

        module FileCfg(Dirs : DIRLIST)(File : FILE) = 
            struct
                include File
                let put = File.put
                let get () =
                    match File.get() with
                    | "" -> default
                    | fn -> fn
                let arg = File.arg
                let register = File.register

                let rec file () =
                    match get () with
                    | f when Sys.file_exists f -> Some f
                    | f -> find f
                and find fn = 
                    Utils.first_file ~dirs:(Dirs.get()) fn

                module FS = Utils.FileSet
                let rec files () =
                    match get () with
                    | f when Sys.file_exists f -> findall (Filename.basename f) (FS.add f FS.empty)
                    | f -> findall (Filename.basename f) FS.empty
                and findall fn fset = 
                    let accum fset d = 
                        let fp = Filename.concat d fn in
                        if Sys.file_exists fp then FS.add fp fset
                        else fset
                    in
                    FS.elements (List.fold_left accum fset (Dirs.get()))

                let cfg_parser cf = 
                    Debug.dprintf "[%s][%d] Parsing [%s]" __FILE__ __LINE__ cf;
                    Env.parse cf;
                    cf

                let parse_file () = 
                    let cf = file () in
                    let () = 
                        match cf with
                        | Some f -> Env.parse f
                        | None -> ()
                    in cf
                and parse_files () = List.map cfg_parser (files())
            end

        module CfgPath : DIRLIST = DirList(
            struct
                let context = ""
                let env = "CFG_PATH"
                let switch = "--cfg-path"
                let desc = "Directory list of possible config files"
                let default = 
                    match try Unix.getenv "HOME" with Not_found -> "" with
                    | "" -> ["."]
                    | home when Utils.dir_eq home "." -> [home]
                    | home -> ["."; home]
            end)
        module CfgFile : FILE = File(
            struct
                let context = ""
                let env = "CFG_FILE"
                let switch = "--cfg-file"
                let desc = "Configuration file"
                let default = 
                    let bn = Filename.basename Sys.argv.(0) in
                    try "."^(Filename.chop_extension bn)
                    with Invalid_argument _ -> "."^bn
            end)
        module ConfigPath(Dirs : DIRLIST)(F : FILE) = FileCfg(Dirs)(F)
        module ConfigFile(F : FILE) = ConfigPath(CfgPath)(F)

        module LoadCfg(F : FILECFG) : LOADCFG = 
            struct
                include F
                let str_file_list ls = 
                    let buf = Buffer.create 1024 in
                    let add f = Buffer.add_string buf f in
                    let next f = Buffer.add_char buf ':'; add f in
                    match ls with
                    | [] -> ""
                    | f :: [] -> f
                    | f :: fs -> 
                        add f;
                        List.iter next fs;
                        Buffer.contents buf 

                let () = Debug.dprintf "[%s] Creating" __MODULE__
                let rec loader (p : unit -> 'a) = 
                    Debug.dprintf "1.  load params";
                    let _ = load_params () in
                    Debug.dprintf "2.  load config files [%s]" (str_file_list (F.files()));
                    ignore (F.parse_files());
                    Debug.dprintf "3.  Re-load params";
                    load_params ()
                and load_file () = loader (fun () -> ignore (F.parse_file()))
                and load_files () = loader (fun () -> ignore (F.parse_files()))
                and load_params () =
                    let rest = Params.parse_params () in
                    Arg.current := 0;
                    rest
                let load () = 
                    Debug.dprintf "loading config files [%s]" (F.to_str());
                    load_files()
            end
        module Configure(F : FILE) = LoadCfg(ConfigFile(F)) 
    end

module MakeStd(E : ENV) = Make(E)(Params)

