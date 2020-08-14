
type elt = private int

type hour = int
type minute = int
type second = int
type hms = hour * minute * second

val zero : elt
val date_add : Date.elt -> elt -> Date.elt

val of_int : int -> elt
val to_int : elt -> int

val of_float : float -> elt
val to_float : elt -> float

val of_int64 : int64 -> elt
val to_int64 : elt -> int64

val to_string : elt -> string
val to_psql_string : elt -> string

val of_ints : hms -> elt
val of_strings : string * string * string -> elt

val time_of_ints : ?hr:hour -> ?min:minute -> ?sec:second -> elt -> elt
val time_of_strs : ?hr:string -> ?min:string -> ?sec:string -> elt -> elt

val hours : elt -> hour
val minutes : elt -> minute

val of_string : string -> elt
val now : unit -> elt

val of_hhmm : string -> elt
val of_hhmmss : string -> elt

val to_hhmm : elt -> string

type time = elt

module type SER = Ser.ELT with type elt = time
module type VAR = 
    sig
        val env : Cfg.env
        val default : elt
        val switch : Cfg.switch
        val desc : Cfg.desc
    end
module type CFG = Cfg.ELT with type elt = time

module Ser : SER
module Cfg(C : VAR) : CFG

module Ini : 
    sig
        module type INI = 
            sig
                include EnvCfg.ELT with type elt = time
                val now : unit -> unit
            end
        module type VAR = sig include EnvCfg.BVAR val default : time end
        module type VAR_STR = sig include EnvCfg.BVAR val default : string end
        module Make(V : VAR) : INI
        module Str(V : VAR_STR) : INI
        module Zero(V : EnvCfg.BVAR) : INI
        module Now(V : EnvCfg.BVAR) : INI
    end

