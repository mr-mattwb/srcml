open Unix
open Printf
open Pervasives

open Utils
module Rxp = Str
module Serial = Ser

exception Of_str of string * string

type env = string
type var = string
type switch = string
type desc = string
type spec = switch * Arg.spec * desc

let putenv a b = Unix.putenv a b
let getenv a = Unix.getenv a

let override_args =
  try Ser.Bool.of_str (getenv "OVERRIDE_ARGS")
  with Not_found -> true

module Args = Map.Make(
  struct
    type t = switch
    let compare = String.compare
  end)
let args = ref Args.empty
let envs = ref Args.empty
let hides = ref Args.empty

let is_hidden sw = 
    try Args.mem sw !hides 
    with Not_found -> false
let hide env sw = hides := Args.add sw env !hides
let hiddens () = List.map fst (Args.bindings !hides)

let get_env v = 
    try getenv v
    with Not_found -> ""

let iter fn = Args.iter (fun k _ -> fn k (get_env k)) !envs
let fold fn init = Args.fold (fun k _ acc -> fn k (get_env k) acc) !envs init

let env_list () = Args.fold (fun k v acc -> k :: acc) !envs [] 
let env_get e = try Args.find e !envs with Not_found -> "", ""
let env_val e = fst (env_get e)
let env_desc e = snd (env_get e)
let env_iter fn = Args.iter fn !envs

let register env ((sw, sp, d) as v) str =
  try
    envs := Args.add env (str, d) !envs;
    let _ = Args.find sw !args in
    if override_args then
      let args' = Args.remove sw !args in
      args := Args.add sw v args'
    else
      eprintf "Cannot override switch:%s\n%!" sw
  with Not_found -> 
    args := Args.add sw v !args
let get_args () = 
    let accum sw v acc = 
        if is_hidden sw then acc
        else v :: acc
    in
    Args.fold accum !args []
let invalid_arg v = 
    eprintf "Invalid argument:  %s\n%!" v;
    exit (-1)
let invalid_argument = "Invalid argument"
let arg__parse args iv ivs =
    if not !Sys.interactive then Arg.parse args iv ivs
let arg_parse () = 
    arg__parse (get_args()) invalid_arg invalid_argument
let arg_parse_extra () =
    let extra = ref [] in
    let add v = extra := v :: !extra in
    arg__parse (get_args()) add invalid_argument;
    List.rev !extra

let desc_str env sw defv desc = sprintf "[%s][%s][%s] %s" env sw defv desc
let output_env fout env (str, d) =
    fprintf fout "# %s\n%s=%s\n%!" d env str
let output fout = Args.iter (output_env fout) !envs
let output_file fname = std_out output fname

let remove env = 
    try 
        Unix.putenv env "";
        args := Args.remove env !args;
        envs := Args.remove env !envs
    with Not_found -> 
        eprintf "Not Found [%s]\n%!" env; 
        ()

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

module Hide(Elt : ELT) =
    struct
        include Elt
        let () = hide Elt.env Elt.switch
    end

module Make(C : Ser.ELT)(E : VAR with type elt = C.elt) =
  struct
    include E

    let rec get_str () = 
        try getenv E.env
        with Not_found ->
        set E.default;
        get_str()

    and get () = 
        let s = get_str() in
        try C.of_str s
        with e -> raise (Of_str (E.env, s))

    and set v = putenv E.env (C.to_str v)

    let desc = desc_str E.env E.switch (get_str()) E.desc

    let env = E.env
    let switch = E.switch
        
    let arg = 
        let of_str v = 
            try set (C.of_str v)
            with e -> raise (Of_str (E.env, v))
        in
        (E.switch, Arg.String of_str, desc)
    let _ = register E.env arg (get_str())
  end

module type STR_VAR = 
  sig
    val env : env
    val default : string
    val switch : switch
    val desc : desc
  end
module Str(E : STR_VAR) =
  struct
    type elt = string
    include E
    let rec get_str () =
      try 
        getenv E.env
      with Not_found ->
        set E.default;
        get_str ()
    and get () = 
        get_str ()
    and set v = 
        putenv E.env v

    let desc = desc_str E.env E.switch (get_str()) E.desc
    let arg = (E.switch, Arg.String set, desc)
    let _ = register E.env arg (get_str())
  end
module Rex(E : STR_VAR) = 
    struct
        include Str(E)
        let rex () = Pcre.regexp ~flags:[`DOTALL] (get())
    end

module type CHR_VAR = 
    sig
        val env : env
        val default : char
        val switch : switch
        val desc : desc
    end
module Chr(E : CHR_VAR) = 
    struct
        type elt = char
        include E
        let set_str v = putenv E.env v
        let rec get_str () = 
            try getenv E.env
            with Not_found -> 
                set_str (String.make 1 E.default);
                get_str ()
        let rec get () = 
            let v = get_str() in
            if v = "" then raise (Failure "Cfg.Chr: (get()) Empty String")
            else v.[0]
        and set v = set_str (String.make 1 v)

        let desc = desc_str E.env E.switch (get_str ()) E.desc
        let arg = (E.switch, Arg.String set_str, desc)
        let _ = register E.env arg (get_str())
    end

module type INT_VAR = 
  sig
    val env : env
    val default : int
    val switch : switch
    val desc : desc
  end
module Int(I : INT_VAR) = Make(Ser.Int)(
  struct
    type elt = int
    include I
  end)

module type INT0_VAR =
    sig
        val env : env
        val switch : switch
        val desc : desc
    end
module Int0(I : INT0_VAR) = Int(
    struct
        include I
        let default = 0
    end)
  
module type INT32_VAR = 
  sig
    val env : env
    val default : int32
    val switch : switch
    val desc : desc
  end
module Int32(I : INT32_VAR) = Make(Ser.Int32)(
  struct
    type elt = int32
    include I
  end)
  
module type INT64_VAR = 
  sig
    val env : env
    val default : int64
    val switch : switch
    val desc : desc
  end
module Int64(I : INT64_VAR) = Make(Ser.Int64)(
  struct
    type elt = int64
    include I
  end)
  
module type FLT_VAR = 
  sig
    val env : env
    val default : float
    val switch : switch
    val desc : desc
  end
module Flt(F : FLT_VAR) = Make(Ser.Flt)(
  struct
    type elt = float
    include F
  end)
  
module type BOOL_VAR = 
  sig
    val env : env
    val default : bool
    val switch : switch
    val desc : desc
  end
module Bool(B : BOOL_VAR) = Make(Ser.Bool)(
  struct
    type elt = bool
    include B
  end)

module type BYTES_VAR = 
  sig
    val env : env
    val default : bytes
    val switch : switch
    val desc : desc
  end
module Bytes(B : BYTES_VAR) = Make(Ser.Bytes)(
    struct
        type elt = bytes
        include B
    end)

module type INET_VAR =
  sig
    val env : env
    val default : inet_addr
    val switch : switch
    val desc : desc
  end
module Inet(I : INET_VAR) = Make(Ser.Inet)(
  struct
    type elt = inet_addr
    include I
  end)

module type INET_DEFAULT =
    sig
        val env : env
        val switch : switch
        val desc : desc
    end
module InetDefault(I : INET_DEFAULT) = Inet(
    struct
        include I
        let default = Utils.inet_default()
    end)
module InetAny(I : INET_DEFAULT) = Inet(
    struct
        include I
        let default = inet_addr_any
    end)

module type FLAG_VAR = BVAR
module type FLAGVAL =
    sig
        val default : bool
    end

module Flag(V : FLAGVAL)(F : FLAG_VAR) = 
    struct
        module Ser = Ser.Bool
        type elt = bool
        include V
        include F
        let bval = ref V.default
        let get () = 
            try 
                let v = Ser.of_str (getenv F.env) in
                bval := v;
                !bval
            with Not_found ->
                !bval
        let set v = 
            putenv F.env (Ser.to_str v);
            bval := v
        let desc = desc_str F.env F.switch (Ser.to_str (get())) F.desc
        let arg = (F.switch, Arg.Unit (fun () -> bval := not V.default), desc)
        let _ = register F.env arg (Ser.to_str (get()))
    end
module Set(F : FLAG_VAR) = Flag(struct let default = false end)(F)
module Clear(F : FLAG_VAR) = Flag(struct let default = true end)(F)

module type HOST_VAR =
    sig
        val env : env
        val default : host
        val switch : switch
        val desc : desc
    end
module Host(H : HOST_VAR) = Str(H)

module type PORT_VAR = 
    sig
        val env : env
        val default : port
        val switch : switch
        val desc : desc
    end
module Port(P : PORT_VAR) = Int(P)

module type EMAIL_VAR =
    sig
        val env : env
        val default : email
        val switch : switch
        val desc : desc
    end
module Email(E : EMAIL_VAR) = Str(E)

let rec parse_line line =
  let buf = Buffer.create 1024 in
  let lex = Lexing.from_string line in
  match Lex.token buf lex with
  | `Eof v  -> putenv v ""
  | `Eq v -> putenv v (parse_in2 buf lex)

and parse_in2 buf lex = 
  match Lex.token buf lex with
  | `Eof v -> v 
  | `Eq v -> 
      let v' = parse_in2 buf lex in
      putenv v v';
      v'

let preprocess fname = "m4 "^fname^" 2>/dev/null"

let parse_file = function
    | "" -> ()
    | fname ->
        try proc_iter parse_line (preprocess fname)
        with e -> () (*  eprintf "Error:%s\n%!" (Utils.error e) *)

module Option(C : Ser.ELT)(E : VAR with type elt = C.elt option) =
  struct
    module CO = Ser.Option(C)
    include E
    let rec get_str () =
        try getenv E.env
        with Not_found -> 
            set E.default;
            get_str()

    and get () = CO.of_str (get_str())
    and set v = putenv E.env (CO.to_str v)

    let desc = desc_str E.env E.switch (get_str()) E.desc
    let arg = (E.switch, Arg.String (fun v -> set (CO.of_str v)), desc)
    let _ = register E.env arg (get_str())
  end
module OptNone(C : Ser.ELT)(E : BVAR) = Option(C)(
    struct
        type elt = C.elt option
        let default = None
        include E
    end)
module StrOpt(E : VAR with type elt = string option) = Option(Ser.Str)(E)

module type SEP = Ser.SEP
module MakeList(Sep : SEP)(Ser : Serial.ELT)(V : VAR with type elt = Ser.elt list) = 
    Make(Serial.MakeList(Sep)(Ser))(V)

module CommaList = MakeList(Serial.Comma)
module SpaceList = MakeList(Serial.Space)
module ColonList = MakeList(Serial.Colon)

module type STRLIST_VAR =
    sig
        val env : env
        val default : string list
        val switch : switch
        val desc : desc
    end
module type STRLIST = ELT with type elt = string list
module CommaStrList(S : STRLIST_VAR) = CommaList(Ser.Str)(
    struct
        type elt = string list
        include S
    end)
module SpaceStrList(S : STRLIST_VAR) = SpaceList(Ser.Str)(
    struct
        type elt = string list
        include S
    end) 
module ColonStrList(S : STRLIST_VAR) = ColonList(Ser.Str)(
    struct
        type elt = string list
        include S
    end)

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
module Tuple(Sep : Serial.SEP)(P1 : Serial.ELT)(P2 : Serial.ELT) =
    struct
        module Ser = Serial.Tuple(Sep)(P1)(P2)
        module type VAR = 
            sig
                val env : env
                val default : Ser.elt
                val switch : switch
                val desc : desc
            end
        module Cfg(V : VAR) = Make(Ser)(
            struct
                type elt = Ser.elt
                include V
            end)
    end
module ColonTuple = Tuple(Ser.Colon)

(*
module CommaList(C : Ser.ELT)(E : VAR with type elt = C.elt list) =
    struct
        type elt = E.elt
        let rxp = Rxp.regexp "[ \t\r\n]*,[ \t\r\n]*"
        let rec get_str () = 
            try getenv E.env
            with Not_found ->
                set E.default;
                get_str()
        and get () = 
            List.map (fun f -> C.of_str f) (Rxp.split rxp (get_str()))
        and set = function
            | [] -> putenv E.env ""
            | x :: [] -> putenv E.env (C.to_str x)
            | x :: xs -> set_str x xs
        and set_str x xs = 
            let v = List.fold_left (fun acc v -> acc^","^(C.to_str v)) (C.to_str x) xs in
            putenv E.env v

        let put_env v = putenv E.env v
        let desc = desc_str E.env E.switch (get_str()) E.desc
        let arg = (E.switch, Arg.String put_env, desc)
        let _ = register E.env arg (get_str())
    end
module SpaceList(C : Ser.ELT)(E : VAR with type elt = C.elt list) =
    struct
        type elt = E.elt
        let rxp = Rxp.regexp "[ \t\r\n]+"
        let rec get_str () = 
            try getenv E.env
            with Not_found ->
                set E.default;
                get_str ()
        and get () = 
            List.map (fun f -> C.of_str f) (Rxp.split rxp (get_str()))
        and set = function
            | [] -> putenv E.env ""
            | x :: [] -> putenv E.env (C.to_str x)
            | x :: xs -> set_str x xs
        and set_str x xs = 
            let v = List.fold_left (fun acc v -> acc^" "^(C.to_str v)) (C.to_str x) xs in
            putenv E.env v

        let put_env v = putenv E.env v
        let desc = desc_str E.env E.switch (get_str()) E.desc
        let arg = (E.switch, Arg.String put_env, desc)
        let _ = register E.env arg (get_str())
    end
*)

module type EMAILS_VAR =
    sig
        val env : env
        val default : email list
        val switch : switch
        val desc : desc
    end
module Emails(E : EMAILS_VAR) = CommaStrList(E)

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
        val dir : unit -> dir
        module File(F : FILE_VAR) : FILE
    end
module type FILE_DIR =
    sig
        include DIR
        module Dir(D : DIR_VAR) : DIR
    end

module File(F : FILE_VAR) = 
    struct
        include Str(F)
        let env = F.env
        let switch = F.switch
        let file () = get()
        let exists () = Sys.file_exists (get())
    end
module Dir(D : DIR_VAR) = 
    struct
        module File' = File
        include Str(D)
        let dir () = get()
        module File(F : FILE_VAR) = 
            struct
                include File'(F)
                let file () = Filename.concat (dir()) (file())
            end
    end
module FileDir(D : DIR_VAR) =
    struct
        module Dirv = Dir(D)
        include Dirv
        module Dir(DV : DIR_VAR) =
            struct
                include Dir(DV)
                let dir () = Filename.concat (Dirv.dir()) (get())
            end
    end
module SubDir(FDir : FILE_DIR)(Sub : DIR_VAR) =
    struct
        module Dir2 = FDir.Dir(Sub)
        include (Dir2 : ELT with type elt = dir)
        let dir = Dir2.dir
        module File(F : FILE_VAR) =
            struct
                module MF = File(F)
                include MF
                let file () = Filename.concat (dir()) (MF.file())
            end
        module Dir(D : DIR_VAR) = 
            struct
                module MD = Dir(D)
                include MD
                let dir () = Filename.concat (dir()) (MD.dir())
            end
    end

module FileOut(F : BVAR) = 
    struct
        include File(
            struct
                let default = ""
                include F
            end)
        let is_stdout () = (get()) = ""
        let use ?(app=true) fn =
            let f = get () in
            match f with
            | ""
            | _ when not app -> Utils.std_out fn f 
            | f -> Utils.file_out_app fn f
    end
module FileIn(F : BVAR) = 
    struct
        include File(
            struct
                let default = ""
                include F
            end)
        let is_stdin () = (get()) = ""
        let use fn = Utils.std_in fn (get())
    end

module FileFind(F : FILE_VAR) = 
    struct
        let fname = 
            try 
                match Unix.getenv F.env with
                | "" -> F.default
                | fe -> fe
            with Not_found -> F.default
        let def = 
            match Utils.find_file fname with
            | None -> fname
            | Some f -> Unix.putenv F.env f; f

        include File(
            struct
                include F
                let default = def
            end)
    end

module type SADDR_VAR = 
    sig
        val env : env
        val default : Unix.sockaddr
        val switch : switch
        val desc : desc
    end
module SockAddr(SA : SADDR_VAR) = Make(Ser.SockAddr)(
    struct
        type elt = Unix.sockaddr
        include SA
    end)
module SockAddrAny(SA : BVAR) = Make(Ser.SockAddr)(
    struct
        type elt = Unix.sockaddr
        let default = ADDR_INET(inet_addr_any, 0)
        include SA
    end)

module type HOSTNAME_VAR = 
    sig
        val env : env
        val switch : switch
        val desc : desc
    end
module Hostname(H : HOSTNAME_VAR) = Str(
    struct
        include H
        let default = gethostname()
    end)

module type CFG_FILE_VAR =
    sig
        val env : env
    end
module ConfigFile(F : CFG_FILE_VAR) = 
    struct 
        let default = ".conf"
        let find fn = 
            match find_file ~env:F.env ~ext:".conf" fn with
            | None -> Utils.touch default; default
            | Some f -> f
        include File(
            struct
                let env = F.env
                let default = find default
                let switch = "--conf"
                let desc = "Configuration file"
            end)
        let old_set = set
        let set fn = old_set (find fn)
    end

module HomeFile(V : BVAR) =
    struct
        let def = 
            let exe = Sys.argv.(0) in
            let no_ext = 
                try Utils.chop_ext exe
                with e -> exe
            in
            "."^(Filename.basename no_ext)
        module F = File(
            struct
                let default = def
                include V
            end)
        include F
        let rec base_file () = 
            let bname = Filename.basename Sys.argv.(0) in
            try Utils.chop_ext bname
            with Invalid_argument _ -> bname
        and file_from_name () = "."^(base_file())
        let rec find () = 
            let ff = F.file() in
            let rec exists () = 
                if Sys.file_exists ff then Some ff
                else try_env (Filename.basename ff)
            and try_env bf = 
                let hf = Filename.concat (Unix.getenv "HOME") bf in
                if Sys.file_exists hf then Some hf
                else None
            in
            match ff with
            | "" -> F.set (file_from_name()); find ()
            | _ -> exists ()
        let get () = F.get()
        let file () = 
            let try_get () = 
                match get() with
                | "" -> file_from_name()
                | fn -> fn
            in
            match find() with
            | None -> try_get()
            | Some fn -> fn
    end

module type RUN =
    sig
        val run : in_channel -> out_channel -> 'a
    end
module type MAIN_IO = 
    sig
        val run : (in_channel -> out_channel -> 'a) -> 'a
    end

module MainIn(In : FILE) = 
    struct
        let run fn = std_in fn (In.file())
    end
module MainOut(Out : FILE) = 
    struct
        let run fn = std_out fn (Out.file())
    end
module MainIO(In : FILE)(Out : FILE) =
    struct
        let run fn = std_io fn (In.file()) (Out.file())
    end
module Main(In : FILE)(Out : FILE) = 
    struct
        module IO = MainIO(In)(Out)
        module Main(Run : RUN) = 
            struct
                let run () = IO.run Run.run
            end
        let fold fn init =
            let fold' fout = std_fold (fn fout) init (In.file()) in
            std_out fold' (Out.file())
        let iter fn = 
            let iter' fout = std_iter (fn fout) (In.file()) in
            std_out iter' (Out.file())
        let map fn = 
            let map' fout = std_map (fn fout) (In.file()) in
            std_out map' (Out.file())
    end

let load_args get = 
    arg_parse();
    parse_file (get());
    arg_parse()

let load_args_extras get = 
    parse_file (get());
    arg_parse_extra()

let load_config get =
    let _ = arg_parse_extra () in
    Arg.current := 0;
    parse_file (get());
    arg_parse_extra()

