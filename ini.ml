open Unix
open Printf
open Pervasives

module PUnix = Unix

open IniBase
open IniLex

exception Switch_overload of string

let empty = Ctx.empty

let add_lex lex cfg = IniLex.process lex cfg
let add_string str cfg = add_lex (Lexing.from_string str) cfg
let add_channel fin cfg = add_lex (Lexing.from_channel fin) cfg
let add_file fn cfg = Utils.file_in (fun fin -> add_channel fin cfg) fn

let init_lex lex = add_lex lex Ctx.empty
let init_string str = add_string str Ctx.empty
let init_channel fin = add_channel fin Ctx.empty
let init_file fname = add_file fname Ctx.empty

let unix_lex ?(ctx="") lex = add_lex lex (init_unix ~ctx ())
let unix_string ?(ctx="") str = add_string str (init_unix ~ctx ())
let unix_channel ?(ctx="") fin = add_channel fin (init_unix ~ctx ())
let unix_file ?(ctx="") fname = add_file fname (init_unix ~ctx ())
(*
-------------------------------------------------------
*)
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
        val set : elt -> unit
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
        val exists : unit -> bool
    end
module type FILE = 
    sig
        include ELT with type elt = Utils.file
        val file : unit -> Utils.file
        val exists : unit -> bool
    end
module type STRLIST = ELT with type elt = string list 
module type SOCKADDR = ELT with type elt = Unix.sockaddr
module type SEC = ELT with type elt = Utils.sec
module type MSEC = ELT with type elt = Utils.msec

module type SIG = 
    sig

        val pIter : context -> (var -> value -> unit) -> props -> unit
        val cIter : (context -> props -> unit) -> unit

        val getenv : ?ctx:context -> var -> value option
        val putenv : ?ctx:context -> var -> value -> unit
        val findenv : ?ctx:context -> var -> value
        val remenv : ?ctx:context -> var -> unit
        val addenv : ?ctx:context -> var -> value -> unit
        val parse_file : Utils.file -> unit

        val buffer : Buffer.t -> unit
        val to_string : unit -> string
        val output : out_channel -> unit
        val print : unit -> unit

        val register : spec -> unit
        val param_list : unit -> spec list
        val parse_params : unit -> string list 
        val load_config : (unit -> Utils.file) -> string list
        val load_file : Utils.file -> unit
        val load_unix : context -> unit

        module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) : ELT with type elt = V.elt
        module Str(V : VAR_STR) : STR
        module StrEmpty(V : BVAR) : STR
        module Int(V : VAR_INT) : INT
        module Int0(V : BVAR) : INT
        module Int32(V : VAR_INT32) : INT32
        module Int64(V : VAR_INT64) : INT64
        module Flt(V : VAR_FLT) : FLT
        module Char(V : VAR_CHAR) : CHAR
        module Bytes(V : VAR_BYTES) : BYTES
        module Bool(V : VAR_BOOL) : BOOL
        module Flag(V : VAR_FLAG)(D : sig val default : bool end) : BOOL
        module Set(V : VAR_FLAG) : FLAG
        module Clear(V : VAR_FLAG) : FLAG
        module Dir(V : VAR_DIR) : DIR
        module File(V : VAR_FILE) : FILE
        module Sec(V : VAR_SEC) : SEC
        module MSec(V : VAR_MSEC) : MSEC
        module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) : ELT with type elt = S.elt option
        module OptNone(S : Ser.ELT)(V : BVAR) : ELT with type elt = S.elt option
        module MakeList(Sep : Ser.SEP)(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = V.elt
        module MakeNull(Sep : Ser.SEP)(S : Ser.ELT)(V : BVAR) : ELT with type elt = S.elt list
        module StrList(Sep : Ser.SEP)(V : VAR_STRLIST) : STRLIST
        module CommaList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module SpaceList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module ColonList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module SemiColonList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module BarList(E : Ser.ELT)(V : VAR with type elt = E.elt list) : ELT with type elt = E.elt list
        module CommaStrList(V : VAR_STRLIST) : STRLIST
        module ColonStrList(V : VAR_STRLIST) : STRLIST
        module SemiColonStrList(V : VAR_STRLIST) : STRLIST
        module SpaceStrList(V : VAR_STRLIST) : STRLIST
        module BarStrList(V : VAR_STRLIST) : STRLIST
        module SockAddr(V : VAR_SOCKADDR) : SOCKADDR
        module SockAny(V : BVAR) : SOCKADDR
    end

module type CTX = sig val ctx : IniBase.context end
module type INIT = sig val cfg : IniBase.cfg end

module Empty = struct let cfg = empty end
module IUnix(Ctx : CTX) = struct let cfg = init_unix ~ctx:Ctx.ctx () end
module IFile(F : Cfg.FILE) = 
    struct
        let cfg = if F.exists() then init_file (F.file()) else empty
    end

module IMake(C : sig val cfg : IniBase.cfg end) : SIG = 
    struct
        let cfg = ref C.cfg

        let pIter ctx fn props = Props.iter fn props
        let cIter fn = Ctx.iter fn !cfg

        let unix_env_sep = try PUnix.getenv "CTX_SEP" with Not_found -> "_" 
        let unix_env_name ctx var = ctx^unix_env_sep^var
        let unix_env_find ctx var = PUnix.getenv (unix_env_name ctx var)
        let unix_env_get ctx var = try Some (unix_env_find ctx var) with Not_found -> None
        let unix_env_put ctx var v = PUnix.putenv (unix_env_name ctx var) v

        let getenv ?(ctx="") var = 
            match IniBase.get_env ~ctx var !cfg with
            | None -> unix_env_get ctx var
            | sv -> sv
        let putenv ?(ctx="") var v = cfg := IniBase.put_env ~ctx var v !cfg
        let findenv ?(ctx="") var = 
            try IniBase.find_env ~ctx var !cfg
            with Not_found -> unix_env_find ctx var
        let remenv ?(ctx="") var = cfg := IniBase.rem_env ~ctx var !cfg
        let addenv ?(ctx="") var s = cfg := IniBase.put_env ~ctx var s !cfg
        let parse_file fname = cfg := add_file fname !cfg

        let buffer b = IniBase.buffer b !cfg
        let to_string () = IniBase.to_string !cfg
        let output fout = IniBase.output fout !cfg
        let print () = IniBase.print !cfg

        module Specs = Set.Make(
            struct
                type t = spec
                let compare (a, _, _) (b, _, _) = String.compare a b
            end)
        let specs = ref Specs.empty
        let register ss = 
            if Specs.mem ss !specs then () (* do nothing *)
            else specs := Specs.add ss !specs
        let update ctx env def = 
            match getenv ~ctx env with
            | None -> addenv ~ctx env def
            | Some s -> addenv ~ctx env s
        let param_list () = Specs.elements !specs
        let parse_params_interactive () = 
            let extras = ref [] in
            let add v = extras := v :: !extras in
            Arg.parse (param_list()) add "Invalid arugment";
            List.rev !extras
        let parse_params () = 
            if !Sys.interactive then []
            else parse_params_interactive()

        (* *)
        let rec load_config get =
            let _ = parse_params () in
            Arg.current := 0;
            load_file (get ());
            parse_params ()
        and load_file = function
            | "" -> ()
            | fn when not (Sys.file_exists fn) -> ()
            | fn -> parse_file fn
        and load_unix ctx = cfg := add_unix ~ctx !cfg

        let make_desc ctx env sw ds = 
            match ctx with
            | ""  -> sprintf "[][%s][%s] %s" env sw ds
            | ctx -> sprintf "[%s][%s][%s] %s" ctx env sw ds

        module Make(S : Ser.ELT)(V : VAR with type elt = S.elt) : ELT with type elt = V.elt =
            struct
                include V
                let rec get () = 
                    match getenv ~ctx:V.context V.env with
                    | None -> set V.default; get()
                    | Some s -> S.of_str s
                and set v = putenv ~ctx:V.context V.env (S.to_str v)
                let desc = make_desc V.context V.env V.switch V.desc
                let set_str s = putenv ~ctx:V.context V.env s
                let arg = (V.switch, Arg.String set_str, desc)
                let _ = 
                    register arg;
                    update V.context V.env (S.to_str V.default)
            end
        module Str(V : VAR_STR) = Make(Ser.Str)(struct type elt = string include V end)
        module StrEmpty(V : BVAR) = Str(struct let default = "" include V end)
        module Int(V : VAR_INT) = Make(Ser.Int)(struct type elt = int include V end)
        module Int0(V : BVAR) = Int(struct let default = 0 include V end)
        module Int32(V : VAR_INT32) = Make(Ser.Int32)(struct type elt = int32 include V end)
        module Int64(V : VAR_INT64) = Make(Ser.Int64)(struct type elt = int64 include V end)
        module Flt(V : VAR_FLT) = Make(Ser.Flt)(struct type elt = float include V end)
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
                    match getenv ~ctx:V.context V.env with
                    | None -> set D.default; get ()
                    | Some v -> Cvt.of_str v
                and set v = putenv ~ctx:V.context V.env (Cvt.to_str v)
                let arg = 
                    let md = make_desc V.context V.env V.switch V.desc in
                    (V.switch, Arg.Unit (fun () -> set (not D.default)), md)
                let _ = 
                    register arg;
                    update V.context V.env (Cvt.to_str D.default)
            end
        module Set(V : VAR_FLAG) = Flag(V)(struct let default = false end)
        module Clear(V : VAR_FLAG) = Flag(V)(struct let default = true end)
        module Dir(V : VAR_DIR) = 
            struct
                include Str(V)
                let dir() = get()
                let exists () = Sys.file_exists(dir())
            end
        module File(V : VAR_FILE) = 
            struct
                include Str(V)
                let file () = get()
                let exists () = Sys.file_exists(file())
            end
        module Sec(V : VAR_SEC) = Int(V)
        module MSec(V : VAR_MSEC) = Int32(V)
        module Option(S : Ser.ELT)(V : VAR with type elt = S.elt option) = 
            struct
                include V
                let get () = 
                    match getenv ~ctx:context env with
                    | None 
                    | Some "" -> default
                    | Some str -> Some (S.of_str str)
                let put s = putenv ~ctx:context env s
                let set = function
                    | None -> remenv ~ctx:context env
                    | Some s -> put (S.to_str s)
                let arg = 
                    let desc = make_desc context env switch desc in
                    let setter s = put s in
                    (V.switch, Arg.String setter, desc)
                let _ = 
                    register arg;
                    update context env ""
            end
        module OptNone(S : Ser.ELT)(V : BVAR) = Option(S)(
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
                let put s = putenv ~ctx:context env s
                let set ls = put (S.to_str ls)
                let arg = 
                    let desc = make_desc context env switch desc in
                    let setter v = put v in
                    (V.switch, Arg.String setter, desc)
                let _ = 
                    register arg;
                    update context env ""
            end
        module MakeNull(Sep : Ser.SEP)(S : Ser.ELT)(V : BVAR) = MakeList(Sep)(S)(
            struct
                type elt = S.elt list
                let default = []
                include V
            end)

        module StrList(Sep : Ser.SEP)(V : VAR_STRLIST) = MakeList(Sep)(Ser.Str)(
            struct
                type elt = string list
                include V
            end)
        module CommaList(E : Ser.ELT)(V : VAR with type elt = E.elt list) = MakeList(Ser.Comma)(E)(V)
        module SpaceList(E : Ser.ELT)(V : VAR with type elt = E.elt list) = MakeList(Ser.Space)(E)(V)
        module ColonList(E : Ser.ELT)(V : VAR with type elt = E.elt list) = MakeList(Ser.Colon)(E)(V)
        module SemiColonList(E : Ser.ELT)(V : VAR with type elt = E.elt list) = MakeList(Ser.SemiColon)(E)(V)
        module BarList(E : Ser.ELT)(V : VAR with type elt = E.elt list) = MakeList(Ser.Bar)(E)(V)
        module CommaStrList(V : VAR_STRLIST) = StrList(Ser.Comma)(V)
        module ColonStrList(V : VAR_STRLIST) = StrList(Ser.Colon)(V)
        module SemiColonStrList(V : VAR_STRLIST) = StrList(Ser.SemiColon)(V)
        module SpaceStrList(V : VAR_STRLIST) = StrList(Ser.Space)(V)
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
    end

include IMake(IUnix(struct let ctx = "default" end))

module type IFILE = 
    sig
        include FILE
        val find : unit -> Utils.file option
    end

module HomeFile(V : BVAR) =
    struct
        let exe = Filename.basename Sys.argv.(0)
        let base_name = 
            try Utils.chop_ext exe
            with e -> exe
        let default_file = "."^base_name

        module F = File(
            struct
                let default = default_file
                include V
            end)
        include F
        let () =  if file () = "" then set default_file else ()

        let rec find () = 
            let bf = file () in
            if Sys.file_exists bf then Some bf
            else try_env bf
        and try_env bn = 
            let pn = Filename.concat (Unix.getenv "HOME") bn in
            if Sys.file_exists pn then Some pn
            else None

        let file () =
            let try_get () =
                match get () with
                | "" -> default_file
                | fn -> fn
            in
            match find () with
            | None -> try_get ()
            | Some fn -> fn
    end

