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

module Ser : SER 
module UVar : UVAR
module CVar : CVAR
module OVar : OVAR
module Spec : SPEC

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

        val sockaddr : Unix.sockaddr OVar.t -> Unix.sockaddr t
        val sockaddr_str : string OVar.t -> Unix.sockaddr t

        val tuple : ('a * 'b) Ser.t -> ('a * 'b) OVar.t -> ('a * 'b) t

        val cfg_path_var : CVar.t       (* CFG_PATH --cfg-path *)
        val cfg_file_var : CVar.t       (* CFG_FILE --cfg-file *)
        val cfg_file : CVar.t -> CVar.t -> file_cfg_t
        val cfg_path : CVar.t -> file_cfg_t   (* CFG_PATH CFG_FILE *)

        val load_cfg : file_cfg_t -> load_cfg_t
        val load_file : CVar.t -> CVar.t -> load_cfg_t
        val load_path : CVar.t -> load_cfg_t
    end

module Make(Env : IniEnv.ENV) : ELT

include ELT
val load : CVar.t -> string list

