open Unix
open Printf
open Pervasives

open Utils

module Rex = Pcre

type context = IniBase.context
type env = string
type switch = string
type desc = string
type spec = switch * Arg.spec * desc

exception Conversion_failure of string

module type SER =
    sig
        class type ['a] t =
            object
                method of_str : string -> 'a
                method to_str : 'a -> string
            end
        class type sep_t = 
            object
                method split : string -> string list
                method rex : Rex.regexp
                method sep : string
            end
        class ['a] elt : (string -> 'a) -> ('a -> string) -> ['a] t
        class str_elt : [string] t
        class int_elt : [int] t
        class int32_elt : [int32] t
        class int64_elt : [int64] t
        class flt_elt : [float] t
        class file_elt : [Utils.file] t
        class dir_elt : [Utils.dir] t
        class bool_elt : [bool] t
        class bytes_elt : [bytes] t
        class char_elt : [char] t
        class sec_elt : [Utils.sec] t
        class msec_elt : [Utils.msec] t
        class inet_elt : [Unix.inet_addr] t
        class host_inet_elt : [Unix.inet_addr] t

        class ['a] option_elt : 'a t -> ['a option] t
        class ['a] list_elt : sep_t -> 'a t -> ['a list] t

        class comma_sep_elt : sep_t
        class space_sep_elt : sep_t
        class colon_sep_elt : sep_t
        class semicolon_sep_elt : sep_t
        class bar_sep_elt : sep_t
        class equal_sep_elt : sep_t

        val comma_sep : sep_t
        val space_sep : sep_t
        val colon_sep : sep_t
        val semicolon_sep : sep_t
        val bar_sep : sep_t
        val equal_sep : sep_t

        class ['a] comma_list_elt : 'a t -> ['a list] t
        class ['a] space_list_elt : 'a t -> ['a list] t
        class ['a] colon_list_elt : 'a t -> ['a list] t
        class ['a] semicolon_list_elt : 'a t -> ['a list] t
        class ['a] bar_list_elt : 'a t -> ['a list] t

        class str_list_elt : sep_t -> [string list] t
        class comma_str_list_elt : [string list] t
        class space_str_list_elt : [string list] t
        class colon_str_list_elt : [string list] t
        class semicolon_str_list_elt : [string list] t
        class bar_str_list_elt : [string list] t

        class dir_set_elt : [DirSet.t] t

        class addr_inet_elt : [inet_addr * int] t
        class sockaddr_inet_elt : [inet_addr * int] t
        class sockaddr_elt : [sockaddr] t

        class ['a,'b] tuple_elt : sep_t -> 'a t -> 'b t -> ['a * 'b] t

        val str : string t
        val integer : int t
        val integer32 : int32 t
        val integer64 : int64 t
        val flt : float t
        val file : Utils.file t
        val dir : Utils.dir t
        val bool : bool t
        val bytes : bytes t
        val char : char t
        val sec : Utils.sec t
        val msec : Utils.msec t
        val inet : Unix.inet_addr t
        val host_inet : Unix.inet_addr t

        val optional : 'a t -> 'a option t

        val comma_list : 'a t -> 'a list t
        val space_list : 'a t -> 'a list t
        val colon_list : 'a t -> 'a list t
        val semicolon_list : 'a t -> 'a list t
        val bar_list : 'a t -> 'a list t

        val comma_str_list : string list t
        val space_str_list : string list t
        val colon_str_list : string list t
        val semicolon_str_list : string list t
        val bar_str_list : string list t

        val dir_set : DirSet.t t

        val addr_inet : (inet_addr * int) t
        val sockaddr_inet : (inet_addr * int) t
        val sockaddr : sockaddr t
        
        val tuple : sep_t -> 'a t -> 'b t -> ('a * 'b) t
    end

module type UVAR =
    sig
        class type t = 
            object
                method env : env
                method switch : switch
                method desc : desc
            end
        class elt : env -> switch -> desc -> t
        class cp_elt : t -> t
    end

module type CVAR =
    sig
        module UVar : UVAR
        class type t =
            object
                inherit UVar.t
                method context : context
            end
        class elt : context -> env -> switch -> desc -> t
        class cp_elt : t -> t
        class cp_uvar_elt : context -> UVar.t -> t
        class uvar_elt : env -> switch -> desc -> t

        val make : context -> env -> switch -> desc -> t
        val uvar : env -> switch -> desc -> t
    end

module type OVAR =
    sig
        module CVar : CVAR
        class type ['a] t =
            object
                inherit CVar.t
                method default : 'a
            end
        class ['a] elt : context -> env -> 'a -> switch -> desc -> ['a] t
        class ['a] cp_elt : 'a t -> ['a] t
        class ['a] default_elt : CVar.t -> 'a -> ['a] t
        class ['a] uvar_elt : env -> switch -> desc -> 'a -> ['a] t
        class int_0_elt : CVar.t -> [int] t
        class int32_0_elt : CVar.t -> [int32] t
        class int64_0_elt : CVar.t -> [int64] t
        class flt_0_elt : CVar.t -> [float] t
        class str_empty_elt : CVar.t -> [string] t
        class ['a] option_none_elt : CVar.t -> ['a option] t
        class ['a] list_null_elt : CVar.t -> ['a list] t
        class ['a] uvar_list_null_elt : env -> switch -> desc -> ['a list] t
        class flag_elt : CVar.t -> bool -> [bool] t
        class set_flag_elt : CVar.t -> [bool] t
        class clear_flag_elt : CVar.t -> [bool] t
        class cwd_elt : CVar.t -> [Utils.dir] t

        val default : CVar.t -> 'a -> 'a t

        val set_flag : CVar.t -> bool t
        val clear_flag : CVar.t -> bool t
        val uvar : env -> switch -> desc -> 'a -> 'a t
        val uvar_list_null : env -> switch -> desc -> 'a list t

        val sockinet_null : env -> switch -> desc -> sockaddr t
        val of_hostname : string t -> inet_addr t
        val addr_of_str : string t -> sockaddr t
    end

module type SPEC =
    sig
        class virtual tt : 
            object
                method virtual arg : spec
            end
        include Set.S with type elt = spec
        val register : tt -> unit

        val env_invalid_arg : string
        val def_invalid_arg : string
        val invalid_arg : string

        val param_list : unit -> spec list
        val parse_cli_params : unit -> string list
        val parse_params : unit -> string list
    end

module Ser : SER = 
    struct
        class type ['a] t =
            object
                method of_str : string -> 'a
                method to_str : 'a -> string
            end
        class type sep_t = 
            object
                method split : string -> string list
                method rex : Rex.regexp
                method sep : string
            end

        class ['a] elt (ofStr : string -> 'a) (toStr : 'a -> string) = 
            object
                method of_str s = ofStr s
                method to_str s = toStr s
            end
        class str_elt = 
            object
                method of_str (s : string) = s
                method to_str (s : string) = s
            end
        class virtual ['a] int_base_elt =
            object (self)
                method private is_digits = 
                    let rex = Pcre.regexp "^\\d+$" in
                    function v -> Pcre.pmatch ~rex v
                method private virtual init : 'a
                method private virtual to_elt : string -> 'a
                method of_str = function
                    | "" -> self#init
                    | n when self#is_digits n -> self#to_elt n
                    | n -> raise (Invalid_argument ("of_string["^n^"]"))
            end
        class int_elt = 
            object
                inherit [int] int_base_elt
                method private init = 0
                method private to_elt s = int_of_string s
                method to_str = string_of_int
            end
        class int32_elt = 
            object (self)
                inherit [int32] int_base_elt
                method private init = 0l
                method private to_elt s = Int32.of_string s
                method to_str s = Int32.to_string s
            end
        class int64_elt = 
            object 
                inherit [int64] int_base_elt
                method private init = 0L
                method private to_elt s = Int64.of_string s
                method to_str s = Int64.to_string s
            end
        class flt_elt = 
            object
                method of_str = function
                    | "" -> 0.0
                    | n -> float_of_string n
                method to_str s = string_of_float s
            end
        class file_elt =
            object
                inherit str_elt
            end
        class dir_elt = 
            object
                inherit str_elt
            end
        class bool_elt = 
            object
                method of_str s = 
                    match s.[0] with
                    | 'F' | 'f' | 'n' | 'N' | '0' -> false
                    | _ -> true
                method to_str b = string_of_bool b
            end
        class bytes_elt = 
            object
                method of_str s = Bytes.of_string s
                method to_str b = Bytes.to_string b
            end
        class char_elt = 
            object
                method of_str = function
                    | "" -> '\000'
                    | n -> n.[0]
                method to_str ch = String.make 1 ch
            end
        class sec_elt = 
            object
                inherit int_elt
            end
        class msec_elt = 
            object
                inherit int32_elt
            end
        class inet_elt =
            object
                method of_str = Unix.inet_addr_of_string
                method to_str = Unix.string_of_inet_addr
            end
        class host_inet_elt = 
            object
                val inet = new inet_elt
                method of_str host = 
                    try (gethostbyname host).h_addr_list.(0)
                    with _ -> inet#of_str host
                method to_str addr =
                    try (gethostbyaddr addr).h_name
                    with _ -> inet#to_str addr
            end

        class ['a] option_elt (s : 'a t) =
            object
                method of_str = function
                    | "" -> None
                    | o -> Some (s#of_str o)
                method to_str = function
                    | None -> ""
                    | Some o -> s#to_str o
            end

        class ['a] list_elt (sep : sep_t) (ser : 'a t) = 
            object
                method of_str str = 
                    let ofstr selt = 
                        try ser#of_str selt
                        with Failure f -> raise (Failure (str^":"^f))
                    in
                    List.map ofstr (sep#split str)
                method to_str ls = 
                    let buf = Buffer.create 1024 in
                    let rec add item = Buffer.add_string buf (ser#to_str item)
                    and next item = Buffer.add_string buf sep#sep; add item
                    and accum item xs = 
                        add item;
                        List.iter next xs;
                        Buffer.contents buf
                    in
                    match ls with 
                    | [] -> ""
                    | x :: [] -> ser#to_str x
                    | x :: xs -> accum x xs 
            end

        let wspc = "[\r\t ]"
        let wspcs = wspc^"*"
        class virtual sep_v =
            object (self)
                method split str = Rex.split ~rex:self#rex str
                method virtual rex : Rex.regexp
                method virtual sep : string
            end
        class comma_sep_elt = 
            object 
                inherit sep_v
                method rex = Rex.regexp (wspcs^","^wspcs)
                method sep = ","
            end
        class space_sep_elt = 
            object
                inherit sep_v
                method rex = Rex.regexp (wspc^"+")
                method sep = " "
            end
        class colon_sep_elt = 
            object
                inherit sep_v
                method rex = Rex.regexp (wspcs^":"^wspcs)
                method sep = ":"
            end
        class semicolon_sep_elt = 
            object
                inherit sep_v
                method rex = Rex.regexp (wspcs^";"^wspcs)
                method sep = ";"
            end
        class bar_sep_elt = 
            object
                inherit sep_v
                method rex = Rex.regexp (wspcs^(Rex.quote "|")^wspcs)
                method sep = "|"
            end
        class equal_sep_elt = 
            object
                inherit sep_v
                method rex = Rex.regexp (wspcs^(Rex.quote "=")^wspcs)
                method sep = "="
            end
        let comma_sep = new comma_sep_elt
        let space_sep = new space_sep_elt
        let colon_sep = new colon_sep_elt
        let semicolon_sep = new semicolon_sep_elt
        let bar_sep = new bar_sep_elt
        let equal_sep = new equal_sep_elt

        class ['a] comma_list_elt (ser : 'a t) = 
            object
                inherit ['a] list_elt comma_sep ser
            end
        class ['a] space_list_elt (ser : 'a t) = 
            object
                inherit ['a] list_elt space_sep ser
            end
        class ['a] colon_list_elt (ser : 'a t) = 
            object
                inherit ['a] list_elt colon_sep ser
            end
        class ['a] semicolon_list_elt (ser : 'a t) = 
            object
                inherit ['a] list_elt semicolon_sep ser
            end
        class ['a] bar_list_elt (ser : 'a t) = 
            object
                inherit ['a] list_elt bar_sep ser
            end

        class str_list_elt (sep : sep_t) = 
            object
                inherit [string] list_elt sep (new str_elt)
            end
        class comma_str_list_elt =
            object
                inherit str_list_elt comma_sep
            end
        class space_str_list_elt = 
            object
                inherit str_list_elt space_sep
            end
        class colon_str_list_elt = 
            object
                inherit str_list_elt colon_sep
            end
        class semicolon_str_list_elt = 
            object
                inherit str_list_elt semicolon_sep
            end
        class bar_str_list_elt = 
            object
                inherit str_list_elt bar_sep
            end

        class dir_set_elt =
            object (self)
                val ser = new colon_str_list_elt
                method private add (set : DirSet.t) (elt : Utils.dir) = DirSet.add elt set
                method of_str str = List.fold_left self#add DirSet.empty (ser#of_str str)
                method to_str set = ser#to_str (DirSet.elements set)
            end

        class virtual ['a] of_str_list_v = 
            object
                method virtual private of_str_list : string list -> 'a
            end

        class addr_inet_elt = 
            object (self)
                inherit [inet_addr * int] of_str_list_v
                val port = new int_elt
                val addr = new host_inet_elt 
                method private null = (inet_addr_any, 0)
                method to_str (ia, p) = sprintf "%s:%d" (addr#to_str ia) p
                method of_str = function
                    | "" ->  self#null
                    | iap -> self#of_str_list (self#split iap)
                method private split =
                    let rex = Rex.regexp ":" in
                    function iap -> Rex.split ~rex iap
                method private of_str_list = function
                    | [] -> self#null
                    | istr :: [] -> (addr#of_str istr, 0)
                    | istr :: ps :: _ -> (addr#of_str istr, port#of_str ps)
            end
        class sockaddr_inet_elt = 
            object (self)
                inherit addr_inet_elt
                method private of_str_list = function
                    | istr :: ps :: _ -> (addr#of_str istr, port#of_str ps)
                    | _ -> raise (Failure "sockaddr_inet_elt:  Port is required")
            end

        class sockaddr_elt = 
            object (self)
                val addr = new sockaddr_inet_elt
                method private unknown fn = 
                    (* Try inet first *)
                    try 
                        let ia, p = addr#of_str fn in
                        ADDR_INET (ia, p)
                    (* If inet does not work, use Unix *)
                    with _ ->
                        ADDR_UNIX fn
                method private addr_of_str ad = 
                    let ia, p = addr#of_str ad in
                    ADDR_INET (ia, p)
                method private of_substr orig subs = function
                    | 'U' -> ADDR_UNIX subs
                    | 'I' -> self#addr_of_str subs 
                    | ch -> self#unknown orig 
                method of_str = function
                    | ""
                    | "U" -> ADDR_UNIX ""
                    | "I" -> ADDR_INET(inet_addr_any, 0)
                    | str -> self#of_substr str (String.sub str 1 (String.length str - 1)) str.[0]
                method to_str = function
                    | ADDR_UNIX "" -> "U"
                    | ADDR_INET (ia, 0) when ia = inet_addr_any -> "I"
                    | ADDR_UNIX fn -> sprintf "U%s" fn
                    | ADDR_INET (ia, p) -> sprintf "I%s" (addr#to_str (ia, p))
            end

        class ['a,'b] tuple_elt (sep : sep_t) (f : 'a t) (s : 'b t) =
            object (self)
                method of_str str = 
                    match sep#split str with
                    | a :: b :: _ -> (f#of_str a, s#of_str b)
                    | _ -> raise (Invalid_argument "OIni.Ser.tuple_elt")
                method to_str (a, b) =
                    sprintf "%s%s%s" (f#to_str a) sep#sep (s#to_str b)
            end

        let str : string t = new str_elt
        let integer : int t = new int_elt 
        let integer32 : int32 t = new int32_elt
        let integer64 : int64 t = new int64_elt
        let flt : float t = new flt_elt
        let file : Utils.file t = new str_elt
        let dir : Utils.dir t = new str_elt
        let bool : bool t = new bool_elt
        let bytes : bytes t = new bytes_elt
        let char : char t = new char_elt
        let sec : Utils.sec t = new sec_elt
        let msec : Utils.msec t = new msec_elt
        let inet : Unix.inet_addr t = new inet_elt
        let host_inet : Unix.inet_addr t = new host_inet_elt

        let optional (ser : 'a t) : 'a option t = new option_elt ser

        let comma_list ser = new comma_list_elt ser
        let space_list ser = new space_list_elt ser
        let colon_list ser = new colon_list_elt ser
        let semicolon_list ser = new semicolon_list_elt ser
        let bar_list ser = new bar_list_elt ser

        let comma_str_list = new comma_str_list_elt
        let space_str_list = new space_str_list_elt
        let colon_str_list = new colon_str_list_elt
        let semicolon_str_list = new semicolon_str_list_elt
        let bar_str_list = new bar_str_list_elt

        let dir_set = new dir_set_elt

        let addr_inet = new addr_inet_elt 
        let sockaddr_inet = new sockaddr_inet_elt
        let sockaddr = new sockaddr_elt

        let tuple sep a b = new tuple_elt sep a b 
    end

module UVar : UVAR = 
    struct
        class type t = 
            object
                method env : env
                method switch : switch
                method desc : desc
            end
        class elt (e : env) (s : switch) (d : desc) = 
            object
                method env = e
                method switch = s
                method desc = d 
            end
        class cp_elt (c : t) = 
            object
                inherit elt c#env c#switch c#desc
            end
    end

module CVar : CVAR = 
    struct
        module UVar = UVar
        class type t = 
            object
                inherit UVar.t
                method context : context
            end
        class elt (c : context) (e : env) (s : switch) (d : desc) = 
            object
                inherit UVar.elt e s d 
                method context = c
            end
        class cp_elt (var : t) = 
            object (self)
                inherit elt var#context var#env var#switch var#desc
            end
        class cp_uvar_elt (c : context) (u : UVar.t) = 
            object
                inherit elt c u#env u#switch u#desc
            end

        class uvar_elt (e : env) (s : switch) (d : desc) =
            object
                inherit cp_uvar_elt "" (new UVar.elt e s d)
            end

        let make c e s d = new elt c e s d
        let uvar e s d = new uvar_elt e s d 
    end

module OVar : OVAR = 
    struct
        module CVar = CVar
        class type ['a] t = 
            object
                inherit CVar.t
                method default : 'a
            end
        class ['a] elt (c : context) (e : env) (def : 'a) (s : switch) (d : desc) =
            object
                inherit CVar.elt c e s d
                method default = def
            end
        class ['a] cp_elt (var : 'a t) = 
            object (self)
                inherit ['a] elt var#context var#env var#default var#switch var#desc
            end
        class ['a] default_elt (var : CVar.t) (def : 'a) =
            object
                inherit ['a] elt var#context var#env def var#switch var#desc
            end
        class ['a] uvar_elt (e : env) (s : switch) (d : desc) (def : 'a) = 
            object
                inherit ['a] default_elt (new CVar.uvar_elt e s d) def
            end
        class int_0_elt (bv : CVar.t) = 
            object
                inherit [int] default_elt bv 0
            end
        class int32_0_elt (bv : CVar.t) =
            object
                inherit [int32] default_elt bv 0l
            end
        class int64_0_elt (bv : CVar.t) =
            object
                inherit [int64] default_elt bv 0L
            end
        class flt_0_elt (bv : CVar.t) = 
            object
                inherit [float] default_elt bv 0.0
            end
        class str_empty_elt (bv : CVar.t) = 
            object
                inherit [string] default_elt bv ""
            end
        class ['a] option_none_elt (bv : CVar.t) = 
            object
                inherit ['a option] default_elt bv None
            end
        class ['a] list_null_elt (bv : CVar.t) = 
            object
                inherit ['a list] default_elt bv []
            end
        class ['a] uvar_list_null_elt (e : env) (s : switch) (d : desc) = 
            object
                inherit ['a list] uvar_elt e s d []
            end
        class flag_elt (bv : CVar.t) (f : bool) = 
            object
                inherit [bool] default_elt bv f
            end
        class set_flag_elt (bv : CVar.t) = 
            object
                inherit flag_elt bv false
            end
        class clear_flag_elt (bv : CVar.t) = 
            object
                inherit flag_elt bv true
            end

        class cwd_elt (bv : CVar.t) = 
            object
                inherit [Utils.dir] default_elt bv (Sys.getcwd())
            end

        let default (bv : CVar.t) def = new default_elt bv def

        let set_flag (bv : CVar.t) = new set_flag_elt bv
        let clear_flag (bv : CVar.t) = new clear_flag_elt bv
        let uvar e s d def = new default_elt (CVar.uvar e s d) def
        let uvar_list_null e s d = new default_elt (CVar.uvar e s d) []

        let sockinet_null e s d = new default_elt (CVar.uvar e s d) (ADDR_INET(inet_addr_any, 0))
        let of_hostname (sv : string t) = new default_elt (sv :> CVar.t) (Ser.host_inet#of_str sv#default)
        let addr_of_str (sv : string t) = new default_elt (sv :> CVar.t) (Ser.sockaddr#of_str sv#default)
    end

module Spec : SPEC =
    struct
        class virtual tt = 
            object
                method virtual arg : spec
            end
        include Set.Make(
            struct
                type t = spec
                let compare (a, _, _) (b, _, _) = String.compare a b
            end)
        let gSpec = ref empty
        let register (ov : tt) = 
            if mem ov#arg !gSpec then ()
            else gSpec := add ov#arg !gSpec

        let env_invalid_arg = "INV_ARG_MSG"
        let def_invalid_arg = "Invalid argument"
        let invalid_arg = 
            try Unix.getenv env_invalid_arg
            with Not_found -> def_invalid_arg

        let param_list () = elements !gSpec
        let parse_cli_params () =
            let extras = ref [] in
            let add v = extras := v :: !extras in
            Arg.parse (param_list()) add invalid_arg;
            List.rev !extras
        let parse_params () =
            if !Sys.interactive then []
            else parse_cli_params ()
    end

module type ELT =
    sig
        class type ['a] t =
            object
                inherit ['a] OVar.t
                inherit ['a] Ser.t
                method get : 'a
                method put : 'a -> unit
                method arg : spec
                method register : unit
            end
        class type file_t = 
            object
                inherit [Utils.file] t
                method file : Utils.file
                method file_ex : Utils.file option
                method exists : bool
                method touch : Utils.file option
            end
        class type dir_t = 
            object
                inherit file_t
                method dir : Utils.dir
                method dir_ex : Utils.dir option
                method make : Unix.file_perm -> Utils.dir option
            end
        class type dir_set_t = 
            object
                inherit [DirSet.t] t
                method to_list : Utils.dir list
                method add : Utils.dir -> unit
                method add_list : Utils.dir list -> unit
            end
        class type file_cfg_t = 
            object
                inherit [Utils.file] t
                method dirs : Utils.dir list t
                method filename : CVar.t
                method basename : Utils.file
                method file : Utils.file option
                method files : Utils.file list
                method parse : Utils.file option
                method parse_all : Utils.file list
            end
        class type load_cfg_t = 
            object
                inherit file_cfg_t
                method load_file : string list
                method load_files : string list
                method load : string list
                method load_params : string list
            end

        val form_desc : context -> env -> switch -> desc -> desc
        val make_desc : CVar.t -> desc

        class ['a] elt : 'a Ser.t -> 'a OVar.t -> ['a] t
        class ['a] option_elt : 'a option Ser.t -> 'a option OVar.t -> ['a option] t
        class ['a] list_elt : 'a list Ser.t -> 'a list OVar.t -> ['a list] t

        class ['a] optional_elt : 'a Ser.t -> 'a option OVar.t -> ['a option] t
        class ['a] list_sep_elt : Ser.sep_t -> 'a Ser.t -> 'a list OVar.t -> ['a list] t

        class str_elt : string OVar.t -> [string] t
        class int_elt : int OVar.t -> [int] t
        class int32_elt : int32 OVar.t -> [int32] t
        class int64_elt : int64 OVar.t -> [int64] t
        class flt_elt : float OVar.t -> [float] t
        class bool_elt : bool OVar.t -> [bool] t
        class bytes_elt : bytes OVar.t -> [bytes] t
        class char_elt : char OVar.t -> [char] t

        class str_empty_elt : CVar.t -> [string] t
        class int_0_elt : CVar.t -> [int] t
        class int32_0_elt : CVar.t -> [int32] t
        class int64_0_elt : CVar.t -> [int64] t
        class flt_0_elt : CVar.t -> [float] t

        class file_elt : Utils.file OVar.t -> file_t
        class dir_elt : Utils.dir OVar.t -> dir_t
        class cwd_elt : CVar.t -> dir_t

        class ['a] option_none_elt : 'a Ser.t -> CVar.t -> ['a option] t
        class ['a] list_null_elt : 'a list Ser.t -> CVar.t -> ['a list] t
        class ['a] list_sep_null_elt : Ser.sep_t -> 'a Ser.t -> CVar.t -> ['a list] t

        class flag_elt : CVar.t -> bool -> [bool] t
        class set_flag_elt : CVar.t -> flag_elt
        class clear_flag_elt : CVar.t -> flag_elt

        class sec_elt : Utils.sec OVar.t -> [Utils.sec] t
        class msec_elt : Utils.msec OVar.t -> [Utils.msec] t
        class inet_elt : Unix.inet_addr OVar.t -> [Unix.inet_addr] t
        class hostname_elt : string OVar.t -> [Unix.inet_addr] t

        class ['a] comma_list_elt : 'a Ser.t -> 'a list OVar.t -> ['a list] t
        class ['a] space_list_elt : 'a Ser.t -> 'a list OVar.t -> ['a list] t
        class ['a] colon_list_elt : 'a Ser.t -> 'a list OVar.t -> ['a list] t
        class ['a] semicolon_list_elt : 'a Ser.t -> 'a list OVar.t -> ['a list] t
        class ['a] bar_list_elt : 'a Ser.t -> 'a list OVar.t -> ['a list] t

        class str_list_elt : Ser.sep_t -> string list OVar.t -> [string list] t
        class comma_str_list_elt : string list OVar.t -> [string list] t
        class space_str_list_elt : string list OVar.t -> [string list] t
        class colon_str_list_elt : string list OVar.t -> [string list] t
        class semicolon_str_list_elt : string list OVar.t -> [string list] t
        class bar_str_list_elt : string list OVar.t -> [string list] t

        class dir_list_elt : Utils.dir list OVar.t -> [Utils.dir list] t
        class file_list_elt : Utils.file list OVar.t -> [Utils.file list] t
        class dir_set_elt : Utils.dir list OVar.t -> [Utils.dir list] t

        class sockaddr_elt : Unix.sockaddr OVar.t -> [Unix.sockaddr] t
        class sockaddr_str_elt : string OVar.t -> [Unix.sockaddr] t

        class ['a,'b] tuple_elt : ('a * 'b) Ser.t -> ('a * 'b) OVar.t -> ['a * 'b] t

        class file_cfg_elt : Utils.dir list t -> CVar.t -> file_cfg_t
        class load_file_elt : Utils.dir list t -> CVar.t -> load_cfg_t
        class load_cfg_elt : file_cfg_t -> load_cfg_t

        val integer : int OVar.t -> int t
        val integer32 : int32 OVar.t -> int32 t
        val integer64 : int64 OVar.t -> int64 t
        val flt : float OVar.t -> float t
        val bool : bool OVar.t -> bool t
        val bytes : bytes OVar.t -> bytes t
        val char : char OVar.t -> char t
        val str_empty : CVar.t -> string t
        val int_0 : CVar.t -> int t
        val int32_0 : CVar.t -> int32 t
        val int64_0 : CVar.t -> int64 t
        val flt_0 : CVar.t -> float t
        val file : Utils.file OVar.t -> file_t
        val dir : Utils.dir OVar.t -> dir_t
        val cwd : CVar.t -> dir_t

        val option_none : 'a Ser.t -> CVar.t -> 'a option t
        val list_null : 'a list Ser.t -> CVar.t -> 'a list t
        val list_sep_null : Ser.sep_t -> 'a Ser.t -> CVar.t -> 'a list t

        val flag : CVar.t -> bool -> bool t
        val set_flag : CVar.t -> bool t
        val clear_flag : CVar.t -> bool t

        val sec : Utils.sec OVar.t -> Utils.sec t
        val msec : Utils.msec OVar.t -> Utils.msec t
        val inet : inet_addr OVar.t -> inet_addr t
        val hostname : string OVar.t -> inet_addr t

        val dir_list : Utils.dir list OVar.t -> dir_list_elt
        val file_list : Utils.file list OVar.t -> file_list_elt
        val dir_set : Utils.dir list OVar.t -> dir_set_elt
        val dir_list_null : CVar.t -> dir_list_elt

        val sockaddr : sockaddr OVar.t -> Unix.sockaddr t
        val sockaddr_str : string OVar.t -> Unix.sockaddr t

        val tuple : ('a * 'b) Ser.t -> ('a * 'b) OVar.t -> ('a * 'b) t

        val cfg_path_var : CVar.t
        val cfg_file_var : CVar.t
        val cfg_file : CVar.t -> CVar.t -> file_cfg_t
        val cfg_path : CVar.t -> file_cfg_t
        val load_cfg : file_cfg_t -> load_cfg_t
        val load_file : CVar.t -> CVar.t -> load_cfg_t
        val load_path : CVar.t -> load_cfg_t
    end

module Make(Env : IniEnv.ENV) = 
    struct
        open Env
        class type ['a] t = 
            object
                inherit ['a] OVar.t
                inherit ['a] Ser.t
                method get : 'a
                method put : 'a -> unit
                method arg : spec
                method register : unit
            end
        class type file_t = 
            object
                inherit [Utils.file] t
                method file : Utils.file
                method file_ex : Utils.file option
                method exists : bool
                method touch : Utils.file option
            end
        class type dir_t = 
            object
                inherit file_t
                method dir : Utils.dir
                method dir_ex : Utils.dir option
                method make : Unix.file_perm -> Utils.dir option
            end 
        class type dir_set_t = 
            object
                inherit [DirSet.t] t
                method to_list : Utils.dir list
                method add : Utils.dir -> unit
                method add_list : Utils.dir list -> unit
            end
        class type file_cfg_t = 
            object
                inherit [Utils.file] t
                method dirs : Utils.dir list t
                method filename : CVar.t
                method basename : Utils.file
                method file : Utils.file option
                method files : Utils.file list
                method parse : Utils.file option
                method parse_all : Utils.file list
            end
        class type load_cfg_t = 
            object
                inherit file_cfg_t
                method load_file : string list
                method load_files : string list
                method load : string list
                method load_params : string list
            end

        let form_desc c e s d = sprintf "[%s][%s][%s] %s" c e s d
        let make_desc var = form_desc var#context var#env var#switch var#desc

        class ['a] elt (ser : 'a Ser.t) (var : 'a OVar.t) =
            let ctx = var#context in
            object (self)
                inherit ['a] OVar.cp_elt var
                inherit ['a] Ser.elt ser#of_str ser#to_str
                inherit Spec.tt 
                method get =
                    match Env.get ~ctx self#env with
                    | None -> self#default
                    | Some str -> ser#of_str str
                method put v = 
                    Env.put ~ctx self#env (ser#to_str v)
                method arg = 
                    let set_str s = self#put (ser#of_str s) in
                    (self#switch, Arg.String set_str, make_desc self)
                method register = Spec.register (self :> Spec.tt)
                initializer
                    self#put self#get
                    (* Spec.register (self :> Spec.tt) *)
            end
        class ['a] option_elt (ser : 'a option Ser.t) (var : 'a option OVar.t) =
            object
                inherit ['a option] elt ser var
            end
        class ['a] list_elt (ser : 'a list Ser.t) (var : 'a list OVar.t) = 
            object  (self)
                inherit ['a list] elt ser var
            end

        class ['a] optional_elt (ser : 'a Ser.t) (var : 'a option OVar.t) = 
            object (self)
                inherit ['a] option_elt (Ser.optional ser) var 
            end
        class ['a] list_sep_elt (sep : Ser.sep_t) (ser : 'a Ser.t) (var : 'a list OVar.t) = 
            object (self)
                inherit ['a list] elt (new Ser.list_elt sep ser) var
            end

        class str_elt (var : string OVar.t) = 
            object
                inherit [string] elt Ser.str var
            end
        class int_elt (var : int OVar.t) =
            object
                inherit [int] elt Ser.integer var
            end
        class int32_elt (var : int32 OVar.t) = 
            object
                inherit [int32] elt Ser.integer32 var
            end
        class int64_elt (var : int64 OVar.t) = 
            object
                inherit [int64] elt Ser.integer64 var
            end
        class flt_elt (var : float OVar.t) = 
            object
                inherit [float] elt Ser.flt var
            end
        class bool_elt (var : bool OVar.t) = 
            object
                inherit [bool] elt Ser.bool var
            end
        class bytes_elt (var : bytes OVar.t) = 
            object
                inherit [bytes] elt Ser.bytes var
            end
        class char_elt (var : char OVar.t) = 
            object
                inherit [char] elt (new Ser.char_elt) var
            end
        class str_empty_elt (var : CVar.t) = 
            object
                inherit str_elt (new OVar.str_empty_elt var)
            end
        class int_0_elt (var : CVar.t) = 
            object
                inherit int_elt (new OVar.int_0_elt var)
            end
        class int32_0_elt (var : CVar.t) = 
            object
                inherit int32_elt (new OVar.int32_0_elt var)
            end
        class int64_0_elt (var : CVar.t) = 
            object
                inherit int64_elt (new OVar.int64_0_elt var)
            end
        class flt_0_elt (var : CVar.t) = 
            object
                inherit flt_elt (new OVar.flt_0_elt var)
            end
        class file_elt (var : Utils.file OVar.t) =
            object (self)
                inherit str_elt var as super
                method file = super#get
                method exists = Sys.file_exists self#file && not (Sys.is_directory self#file)
                method file_ex =
                    if self#exists then Some self#file
                    else None
                method touch =
                    let do_nothing fin = () in
                    let create f = 
                        try Utils.file_in do_nothing f; Some f 
                        with _ -> None
                    in
                    let openf f = 
                        try Utils.file_out_app do_nothing f; Some f 
                        with _ -> None
                    in
                    match self#file_ex with 
                    | None -> create self#file
                    | Some f -> openf f
            end
        class dir_elt (var : Utils.dir OVar.t) = 
            object (self)
                inherit file_elt var
                method dir = self#get
                method exists = Sys.file_exists self#dir && Sys.is_directory self#dir
                method dir_ex = 
                    if self#exists then Some self#dir
                    else None
                method make perms = 
                    let create d = 
                        try Unix.mkdir d perms; Some d
                        with _ -> None
                    in
                    match self#dir_ex with
                    | None -> create self#dir
                    | Some d -> Some d 
            end
        class cwd_elt (var : CVar.t) = 
            object
                inherit dir_elt (new OVar.cwd_elt var)
            end

        class ['a] option_none_elt (ser : 'a Ser.t) (var : CVar.t) = 
            object
                inherit ['a] optional_elt ser (new OVar.option_none_elt var)
            end
        class ['a] list_null_elt (ser : 'a list Ser.t) (var : CVar.t) = 
            object
                inherit ['a list] elt ser (new OVar.default_elt var [])
            end
        class ['a] list_sep_null_elt (sep : Ser.sep_t) (ser : 'a Ser.t) (var : CVar.t) = 
            object
                inherit ['a] list_null_elt (new Ser.list_elt sep ser) var
            end

        class flag_elt (var : CVar.t) (f : bool) = 
            object (self)
                inherit bool_elt (new OVar.flag_elt var f)
                method arg = 
                    let set_arg () = self#put f in
                    (self#switch, Arg.Unit set_arg, make_desc self)
            end
        class set_flag_elt (var : CVar.t) = 
            object (self)
                inherit flag_elt var true
            end
        class clear_flag_elt (var : CVar.t) = 
            object (self)
                inherit flag_elt var false
            end
        class sec_elt (var : Utils.sec OVar.t) = 
            object
                inherit [Utils.sec] elt Ser.sec var
            end
        class msec_elt (var : Utils.msec OVar.t) = 
            object
                inherit [Utils.msec] elt Ser.msec var
            end
        class inet_elt (var : Unix.inet_addr OVar.t) = 
            object
                inherit [Unix.inet_addr] elt Ser.inet var
            end
        class hostname_elt (var : string OVar.t) = 
            object
                inherit [Unix.inet_addr] elt Ser.host_inet (OVar.of_hostname var)
            end

        class ['a] comma_list_elt (ser : 'a Ser.t) (var : 'a list OVar.t) = 
            object
                inherit ['a] list_sep_elt Ser.comma_sep ser var
            end
        class ['a] space_list_elt (ser : 'a Ser.t) (var : 'a list OVar.t) = 
            object
                inherit ['a] list_sep_elt Ser.space_sep ser var
            end
        class ['a] colon_list_elt (ser : 'a Ser.t) (var : 'a list OVar.t) = 
            object
                inherit ['a] list_sep_elt Ser.colon_sep ser var 
            end
        class ['a] semicolon_list_elt (ser : 'a Ser.t) (var : 'a list OVar.t) = 
            object
                inherit ['a] list_sep_elt Ser.semicolon_sep ser var 
            end
        class ['a] bar_list_elt (ser : 'a Ser.t) (var : 'a list OVar.t) = 
            object
                inherit ['a] list_sep_elt Ser.bar_sep ser var 
            end

        class str_list_elt (sep : Ser.sep_t) (var : string list OVar.t) = 
            object
                inherit [string] list_sep_elt sep Ser.str var 
            end
        class comma_str_list_elt (var : string list OVar.t) = 
            object
                inherit str_list_elt Ser.comma_sep var
            end
        class space_str_list_elt (var : string list OVar.t) = 
            object
                inherit str_list_elt Ser.space_sep var
            end
        class colon_str_list_elt (var : string list OVar.t) = 
            object
                inherit str_list_elt Ser.colon_sep var
            end
        class semicolon_str_list_elt (var : string list OVar.t) = 
            object
                inherit str_list_elt Ser.semicolon_sep var
            end
        class bar_str_list_elt (var : string list OVar.t) = 
            object
                inherit str_list_elt Ser.bar_sep var
            end

        class dir_list_elt (v : Utils.dir list OVar.t) =
            object
                inherit [Utils.dir] colon_list_elt Ser.dir v
            end
        class file_list_elt (v : Utils.file list OVar.t) = 
            object
                inherit [Utils.file] colon_list_elt Ser.file v
            end
        class dir_set_elt (v : Utils.dir list OVar.t) = 
            object (self)
                inherit dir_list_elt v as super
                method private add set elt = DirSet.add elt set
                method get = DirSet.elements (List.fold_left self#add DirSet.empty super#get)
            end

        class sockaddr_elt (var : sockaddr OVar.t) = 
            object
                inherit [sockaddr] elt Ser.sockaddr var
            end
        class sockaddr_str_elt (var : string OVar.t) = 
            object
                inherit sockaddr_elt (OVar.addr_of_str var) as super
            end

        class ['a,'b] tuple_elt (ser : ('a * 'b) Ser.t) (var : ('a * 'b) OVar.t) =
            object
                inherit ['a * 'b] elt ser var
            end

        class file_cfg_elt (d : Utils.dir list t) (fname : CVar.t) =
            object (self)
                inherit str_empty_elt fname as super
                method dirs = d
                method filename = fname
                method basename = Filename.basename super#get 
                method private find fname = Utils.first_file ~dirs:self#dirs#get self#basename
                method private find_all fname = List.rev (Utils.files_exist ~dirs:self#dirs#get self#basename)
                method default = "." ^ (Filename.basename Sys.argv.(0))
                method get =
                    match super#get with
                    | "" -> self#default
                    | fn -> fn
                method file =
                    match self#get with
                    | f when Sys.file_exists f -> Some f
                    | f -> self#find f
                method files = 
                    match self#get with
                    | f when Sys.file_exists f -> List.rev (f :: (self#find_all f))
                    | f -> self#find_all f
                method parse =
                    let cf = self#file in
                    let () = 
                        match cf with
                        | Some f  -> Env.parse f
                        | None -> ()
                    in cf
                val dbg = new set_flag_elt (
                    object
                        method context = ""
                        method env = "DEBUG"
                        method switch = "--debug"
                        method desc = "Extreme debugging turns on"
                    end)
                method parse_all = 
                    let cfile cf = 
                        if dbg#get then eprintf "Parsing config file [%s]\n%!" cf else ();
                        Env.parse cf; 
                        cf 
                    in
                    List.map cfile self#files
            end

        class load_file_elt (d : Utils.dir list t) (fname : CVar.t) = 
            object (self)
                inherit file_cfg_elt d fname as super
                method private loader (p : unit -> 'a) = 
                    let _ = self#load_params in
                    p();
                    self#load_params
                method load_file = self#loader (fun () -> ignore self#parse)
                method load_files = self#loader (fun () -> ignore self#parse_all)
                method load = self#load_files
                method load_params = 
                    let rest = Spec.parse_params () in
                    Arg.current := 0;
                    rest
            end
        class load_cfg_elt (cfg : file_cfg_t) = 
            object
                inherit load_file_elt cfg#dirs cfg#filename
            end

        let integer var = new int_elt var
        let integer32 var = new int32_elt var
        let integer64 var = new int64_elt var
        let flt var = new flt_elt var
        let bool var = new bool_elt var
        let bytes var = new bytes_elt var
        let char var = new char_elt var
        let str_empty bvar = new str_empty_elt bvar
        let int_0 bvar = new int_0_elt bvar
        let int32_0 bv = new int32_0_elt bv
        let int64_0 bv = new int64_0_elt bv
        let flt_0 bv = new flt_0_elt bv
        let file var = new file_elt var
        let dir var = new dir_elt var
        let cwd var = new cwd_elt var

        let option_none ser var = new option_none_elt ser var
        let list_null ser var = new list_null_elt ser var
        let list_sep_null sep ser var = new list_sep_null_elt sep ser var

        let flag var def = new flag_elt var def
        let set_flag var = new set_flag_elt var
        let clear_flag var = new clear_flag_elt var

        let sec var = new sec_elt var
        let msec var = new msec_elt var
        let inet var = new inet_elt var
        let hostname var = new hostname_elt var

        let dir_list var = new dir_list_elt var
        let file_list var = new file_list_elt var
        let dir_list_null cvar = new dir_list_elt (OVar.default cvar [])
        let dir_set var = new dir_set_elt var

        let sockaddr var = new sockaddr_elt var
        let sockaddr_str var = new sockaddr_str_elt var

        let tuple ser var = new tuple_elt ser var

        let cfg_path_var = CVar.uvar "CFG_PATH" "--cfg-path" "Directory list of possible config files"
        let cfg_file_var = CVar.uvar "CFG_FILE" "--cfg-file" "Configuraiton file name"
        let cfg_file pd cf = new file_cfg_elt (dir_list_null pd) cf
        let cfg_path cf = cfg_file cfg_path_var cf

        let load_cfg cfg = new load_cfg_elt cfg
        let load_file pd cf = new load_file_elt (dir_list_null pd) cf
        let load_path cf = load_file cfg_path_var cf
    end

include Make(IniEnv.UEnv)
let load cvar = 
    let cfg = load_path cvar in
    cfg#load

