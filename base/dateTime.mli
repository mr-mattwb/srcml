
type elt = private float
type datetime = elt

val to_float : elt -> float
val of_float : float -> elt
val to_int64 : elt -> int64

val zero : elt
val now : unit -> elt
val of_tm : Unix.tm -> elt
val of_date : Date.elt -> elt
val to_date : elt -> Date.elt
val add_time : elt -> Time.elt -> elt
val of_time : Time.elt -> elt

val add_days : days:Date.day -> elt -> elt

val of_string : string -> elt

val of_yyyymmdd : string -> elt
val of_yyyy_mm_dd : string -> elt

val of_ddmmmyyyy : string -> elt
val of_ddmmmyyyy_hhmm : string -> elt

module type SER = Ser.ELT with type elt = datetime
module type VAR =
    sig
        val env : Cfg.env
        val default : elt
        val switch : Cfg.switch
        val desc : Cfg.desc
    end
module type CFG = Cfg.ELT with type elt = datetime

module Ser : SER
module Cfg(C : VAR) : CFG

module Ini : 
    sig
        module type INI = 
            sig
                include EnvCfg.ELT with type elt = datetime
                val now : unit -> unit
            end
        module type VAR = sig include EnvCfg.BVAR val default : datetime end
        module type VAR_STR = sig include EnvCfg.BVAR val default : string end
        module Make(V : VAR) : INI
        module Str(V : VAR_STR) : INI
        module Zero(V : EnvCfg.BVAR) : INI
        module Now(V : EnvCfg.BVAR) : INI
    end
