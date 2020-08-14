open Unix
open Printf
open Pervasives

let from_lex lex = IniLex.uprocess lex
let from_string str = from_lex (Lexing.from_string str) 
let from_channel fin = from_lex (Lexing.from_channel fin)
let from_file fn = Utils.file_in from_channel fn

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

let default_ctx = IniBase.default_context
let env_name = IniBase.context_name

let getenv ?(ctx=default_ctx) var = 
    try Some (Unix.getenv (env_name ctx var))
    with Not_found -> None
let putenv ?(ctx=default_ctx) var dv =
    Unix.putenv (env_name ctx var) dv
let rec findenv ?(ctx=default_ctx) var def = 
    match getenv ~ctx var with
    | None -> putenv ~ctx var def; findenv ~ctx var def
    | Some v -> v
let remenv ?(ctx=default_ctx) env = 
    match getenv ~ctx env with
    | None
    | Some "" -> ()
    | Some _ -> putenv ~ctx env ""
let parse fn = from_file fn

module Specs = Set.Make(
    struct
        type t = spec
        let compare (a, _, _) (b, _, _) = String.compare a b
    end)
let gSpecs = ref Specs.empty
let register ss = 
    if Specs.mem ss !gSpecs then ()
    else gSpecs := Specs.add ss !gSpecs

let update ctx env def = 
    match getenv ~ctx env with
    | None -> putenv ~ctx env def
    | Some _ -> ()

let param_list () = Specs.elements !gSpecs
let parse_params_interactive () =
    let extras = ref [] in
    let add v = extras := v :: !extras in
    Arg.parse (param_list()) add "Invalid argument";
    List.rev !extras
let parse_params () =
    if !Sys.interactive then []
    else parse_params_interactive()

let rec load_config get = 
    let _ = parse_params () in
    Arg.current := 0;
    load_file (get());
    parse_params ()
and load_file = function
    | "" -> ()
    | fn when not (Sys.file_exists fn) -> ()
    | fn -> parse fn

let make_desc ctx env sw ds = sprintf "[%s][%s][%s] %s" ctx env sw ds

module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) =
    struct
        include V

        let rec get () =
            match getenv ~ctx:context env with
            | None -> put default; get ()
            | Some s -> S.of_str s
        and put v = 
            putenv ~ctx:context env (S.to_str v)

        let desc = make_desc context env switch desc
        let set_str s = putenv ~ctx:context env s
        let arg = (switch, Arg.String set_str, desc)
        let _ =
            register arg;
            update context env (S.to_str default)
    end
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
module Bytes(V : VAR_BYTES) = Make(Ser.Bytes)(struct type elt = bytes include V end)
module Bool(V : VAR_BOOL) = Make(Ser.Bool)(struct type elt = bool include V end)
module Flag(V : VAR_FLAG)(D : sig val default : bool end) = 
    struct
        include V
        include D
        module Cvt = Ser.Bool
        type elt = bool
        let rec get () =
            match getenv ~ctx:context env with
            | None -> put default; get ()
            | Some v -> Cvt.of_str v
        and put v = putenv ~ctx:context env (Cvt.to_str v)
        and arg = 
            let md = make_desc context env switch desc in
            (V.switch, Arg.Unit (fun () -> put (not default)), md)
        let _ = 
            register arg;
            update context env (Cvt.to_str default)
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
    end
module File(V : VAR_FILE) = 
    struct
        include Str(V)
        let rec file () = get ()
        and file_ex () = 
            if exists () then Some (file())
            else None
        and exists () = Sys.file_exists (file())
    end
module Sec(V : VAR_SEC) = Int(V)
module MSec(V : VAR_MSEC) = Int32(V)
module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) =
    struct
        include V
        let default_str = 
            match default with
            | None -> ""
            | Some s -> S.to_str s
        let rec get () =
            match getenv ~ctx:context env with
            | None  -> None
            | Some str -> Some (S.of_str str)
        and put = function
            | None -> putenv ~ctx:context env default_str
            | Some s -> putenv ~ctx:context env (S.to_str s)
        let arg = 
            let desc = make_desc context env switch desc in
            let setter str = putenv ~ctx:context env str in
            (switch, Arg.String setter, desc)
        let _ = register arg
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
            match getenv ~ctx:context env with
            | None -> default
            | Some str -> S.of_str str
        let put_str s = putenv ~ctx:context env s
        let put = function
            | [] -> put_str ""
            | ls -> put_str (S.to_str ls)
        let arg = 
            let desc = make_desc context env switch desc in
            let setter v = putenv ~ctx:context env v in
            (V.switch, Arg.String setter, desc)
        let _ = register arg
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

module SockAddr(V : VAR_SOCKADDR) = Make(Ser.SockAddr)(
    struct
        type elt = sockaddr
        include V
    end)
module SockAny(V : BVAR) = SockAddr(
    struct
        let default = ADDR_INET(inet_addr_any, 0)
        include V
    end)

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

module DirList(V : BVAR) = ListNull(Ser.Colon)(Ser.Dir)(V)
module FileList(V : BVAR) = ListNull(Ser.Colon)(Ser.File)(V)

module Config(D : DIRLIST)(V : BVAR) : FILECFG = 
    struct
        module S = StrEmpty(V)
        type elt = Utils.file
        include V
        let defaultfile = "." ^ (Filename.basename Sys.argv.(0))
        let default = defaultfile
        let get () = 
            match S.get() with
            | "" -> default
            | fn -> fn
        let put f = S.put f
        let arg = S.arg
        let rec file () =
            match get () with
            | f when Sys.file_exists f -> Some f
            | f -> find f
        and find fn = 
            Utils.first_file ~dirs:(D.get()) fn

        let rec files () =
            match get () with
            | f when Sys.file_exists f -> f :: (findall f)
            | f -> findall f
        and findall f = 
            Utils.files_exist ~dirs:(D.get()) f

        let parse_file () =
            let x = file () in
            let () = 
                match x with
                | Some f -> parse f
                | None -> ()
            in x
        let parse_files () =
            let pfile pf = 
                parse pf;
                pf
            in List.map pfile (files())
    end

module CfgDirsVar = 
    struct
        let context = ""
        let env = "CFG_PATH"
        let switch = "--cfg-path"
        let desc = "Directory list of possible configuration files"
    end
module CfgFileVar =
    struct
        let context = ""
        let env = "CFG_FILE"
        let switch = "--cfg-file"
        let desc = "Configuration file"
    end
module ConfigFile(D : BVAR)(F : BVAR) = Config(DirList(D))(F)
module CfgPath(F : BVAR) = ConfigFile(CfgDirsVar)(F)

