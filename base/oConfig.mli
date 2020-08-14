open Unix
open Printf
open Pervasives

open IniBase
open OSer
open OVar

module Specs : Set.S with type elt = OVar.spec

class type config_elt =
    object
        method add_ini_file : Utils.file -> unit
        method try_ini_file : Utils.file -> unit
        method add_unix_env : unit -> unit
    end

class type access_elt = 
    object
        inherit config_elt
        method get : evar_elt -> value option
        method put : evar_elt -> value -> unit
        method find : evar_elt -> value
        method rem : evar_elt -> unit
        method add : evar_elt -> value -> unit
        method update : bvar_elt -> value -> unit
    end

class type out_elt =
    object
        inherit access_elt
        method buffer : Buffer.t -> unit
        method to_string : unit -> string
        method output : out_channel -> unit
        method print : unit -> unit
    end

class type cfg_elt = 
    object
        inherit out_elt 
        method register : spec -> unit
        method param_list : unit -> spec list
    end

class type ['a] env_elt = 
    object
        method config : cfg_elt
        method var : 'a OVar.elt
        method env : string
        method ser : 'a OSer.elt
        method desc : desc
        method get : unit -> 'a
        method put : 'a -> unit
        method arg : spec
    end

class type ['a] elt = 
    object
        inherit ['a] env_elt
    end

class type flag_elt = 
    object
        inherit [bool] elt
    end

class type dir_elt = 
    object
        inherit [Utils.dir] elt
        method dir : Utils.dir
        method exists : bool
    end

class type file_elt = 
    object
        inherit [Utils.file] elt
        method file : Utils.file
        method exists : bool
    end

class type env_file_elt = 
    object
        inherit [Utils.file] env_elt
        method file : Utils.file
        method exists : bool
    end


class config_t : unit -> config_elt
class access_t : unit -> access_elt
class out_t : unit -> out_elt
class cfg_t : unit -> cfg_elt

val parse_params : cfg_elt -> string list
val params_interactive : cfg_elt -> string list

class ['a] t : 'a OSer.elt -> 'a OVar.elt -> ['a] elt

class dir_t : Utils.dir OVar.elt -> dir_elt
class file_t : Utils.file OVar.elt -> file_elt
class env_file_t : Utils.file OVar.elt -> env_file_elt
class flag_t : bvar_elt -> bool -> flag_elt

class str_t : string OVar.elt -> 
    object
        inherit [string] elt
    end
class int_t : int OVar.elt -> 
    object
        inherit [int] elt
    end 
class int0_t : bvar_elt -> 
    object
        inherit [int] elt
    end
class int32_t : int32 OVar.elt -> 
    object
        inherit [int32] elt
    end
class int64_t : int64 OVar.elt -> 
    object
        inherit [int64] elt
    end
class flt_t : float OVar.elt -> 
    object
        inherit [float] elt
    end
class chr_t : char OVar.elt -> 
    object
        inherit [char] elt
    end
class bool_t : bool OVar.elt -> 
    object
        inherit [bool] elt
    end
class bytes_t : bytes OVar.elt -> 
    object
        inherit [bytes] elt
    end
class flag_set_t : bvar_elt -> 
    object
        inherit [bool] elt
    end
class flag_clear_t : bvar_elt -> 
    object
        inherit [bool] elt
    end
class ['a] option_t : 'a OSer.elt -> string -> 'a option OVar.elt -> 
    object
        inherit ['a option] elt
    end
class ['a] option_none_t : 'a OSer.elt -> string -> OVar.bvar_elt -> 
    object
        inherit ['a option] elt
    end
class ['a] opt_none_t : 'a OSer.elt -> bvar_elt -> 
    object
        inherit ['a option] elt
    end

class ['a] list_t : 'a OSer.elt -> OSer.sep_elt -> 'a list OVar.elt -> 
    object
        inherit ['a list] elt
    end
class ['a] list_null_t : 'a OSer.elt -> OSer.sep_elt -> bvar_elt -> 
    object
        inherit ['a list] elt
    end
class str_list_t : OSer.sep_elt -> string list OVar.elt -> 
    object
        inherit [string list] elt
    end
class ['a] comma_list_t : 'a OSer.elt -> 'a list OVar.elt ->
    object
        inherit ['a list] elt
    end
class ['a] space_list_t : 'a OSer.elt -> 'a list OVar.elt ->
    object
        inherit ['a list] elt
    end
class ['a] colon_list_t : 'a OSer.elt -> 'a list OVar.elt ->
    object
        inherit ['a list] elt
    end
class ['a] bar_list_t : 'a OSer.elt -> 'a list OVar.elt ->
    object
        inherit ['a list] elt
    end

class inet_t : inet_addr OVar.elt -> 
    object
        inherit [inet_addr] elt
    end
class inet_any_t : bvar_elt ->
    object
        inherit inet_t 
    end

class ['a,'b] tuple_t : OSer.sep_elt -> 'a OSer.elt -> 'b OSer.elt -> ('a * 'b) OVar.elt ->
    object
        inherit ['a * 'b] elt
        method fst : 'a
        method snd : 'b
    end
class ['a,'b] comma_tuple_t : 'a OSer.elt -> 'b OSer.elt -> ('a * 'b) OVar.elt ->
    object
        inherit ['a,'b] tuple_t
    end 
class ['a,'b] space_tuple_t : 'a OSer.elt -> 'b OSer.elt -> ('a * 'b) OVar.elt ->
    object
        inherit ['a,'b] tuple_t
    end
class ['a,'b] colon_tuple_t : 'a OSer.elt -> 'b OSer.elt -> ('a * 'b) OVar.elt ->
    object
        inherit ['a,'b] tuple_t
    end
class ['a,'b] semicolon_tuple_t : 'a OSer.elt -> 'b OSer.elt -> ('a * 'b) OVar.elt ->
    object
        inherit ['a,'b] tuple_t
    end
class ['a,'b] bar_tuple_t : 'a OSer.elt -> 'b OSer.elt -> ('a * 'b) OVar.elt ->
    object
        inherit ['a,'b] tuple_t
    end

val config_file : Utils.file OVar.elt -> file_elt
val home_file : bvar_elt -> env_file_elt

class type base_elt = 
    object
        method config : cfg_elt
        method program : string
        method ident : string
        method args : string list
        method error : exn -> string
        method init : file_elt -> unit
    end
class base_t : 
    object
        inherit base_elt
    end

val new_str : string OVar.elt -> str_t
val new_int : int OVar.elt -> int_t
val new_int0 : OVar.bvar_elt -> int0_t
val new_int32 : int32 OVar.elt -> int32_t
val new_int64 : int64 OVar.elt -> int64_t
val new_flt : float OVar.elt -> flt_t
val new_chr : char OVar.elt -> chr_t
val new_bool : bool OVar.elt -> bool_t
val new_bytes : bytes OVar.elt -> bytes_t
val new_flag_set : OVar.bvar_elt -> bool_t
val new_flag_clear : OVar.bvar_elt -> bool_t

val make_str : context -> env -> string -> switch -> desc -> str_t
val make_int : context -> env -> int -> switch -> desc -> int_t
val make_int0 : context -> env -> switch -> desc -> int_t
val make_int32 : context -> env -> int32 -> switch -> desc -> int32_t
val make_int64 : context -> env -> int64 -> switch -> desc -> int64_t
val make_flt : context -> env -> float -> switch -> desc -> flt_t
val make_chr : context -> env -> char -> switch -> desc -> chr_t
val make_bool : context -> env -> bool -> switch -> desc -> bool_t
val make_bytes : context -> env -> bytes -> switch -> desc -> bytes_t
val flag_set : context -> env -> switch -> desc -> bool_t
val flag_clear : context -> env -> switch -> desc -> bool_t
val make_home_file : context -> env -> switch -> desc -> env_file_t

