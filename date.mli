
module YearSplit : EnvCfg.INT

type elt = private float
type time = elt

type year = private int
type month = private int
type day = private int

type mon = 
    | Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec

type date = private
    {
        year : year;
        month : month;
        day : day
    }

val of_float : float -> elt
val to_float : elt -> float
val to_int64 : elt -> int64
val to_int64_str : elt -> string

val to_psql_string : elt -> string

val string_of_mon : mon -> string
val mon_of_string : string -> mon
val month_of_mon : mon -> month
val mon_of_month : month -> mon
val int_of_day : day -> int
val day_of_int : int -> day
val day_of_string : string -> day
val day_seconds : float
val seconds_of_days : day -> float
val int_of_month : month -> int
val int_of_day : day -> int

val year_of_string : string -> year
val string_of_year : year -> string

val yr_of_string : string -> year


val tm_of_date : date -> Unix.tm
val time_of_date : date -> elt
val tm_of_time : elt -> Unix.tm


val is_leap_year : date -> bool

val date_of_ints : year:int -> month:int -> day:int -> date
val time_of_ints : year:int -> month:int -> day:int -> elt
val to_date : elt -> date

val time_of_ddmmmyyyy : string -> elt
val of_yyyy_mm_dd : string -> elt


val zero : elt
val day_seconds : elt 
val now : unit -> elt 
val today : unit -> elt
val yesterday : unit -> elt
val past : elt -> bool
val future : elt -> bool
val future_day : elt -> bool
val is_today : elt -> bool

val eq : elt -> elt -> bool
val lt : elt -> elt -> bool
val le : elt -> elt -> bool
val (|<|) : elt -> elt -> bool
val (|<=|) : elt -> elt -> bool
val lt_today : elt -> bool
val le_today : elt -> bool

val add_days : days:day -> elt -> elt

val to_slash_string : elt -> string
val of_dd_mmm_yyyy : string -> elt

val of_yyyymmdd : string -> elt
val of_yyyy_mm_dd : string -> elt
val of_mm_dd_yyyy : string -> elt

val get_day : elt -> day

module type DAY_VAR =
    sig
        open Cfg
        val env : env
        val default : day
        val switch : switch
        val desc : desc
    end
module type DAY = Cfg.ELT with type elt = day
module Day(E : DAY_VAR) : DAY

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
module Config(C : VAR) : CFG

module Ini : 
    sig
        module type INI = EnvCfg.ELT with type elt = time
        module type VAR = sig include EnvCfg.BVAR val default : time end
        module type VAR_STR = sig include EnvCfg.BVAR val default : string end
        module Make(V : VAR) : INI
        module Str(V : VAR_STR) : INI
        module Zero(V : EnvCfg.BVAR) : INI
        module Now(V : EnvCfg.BVAR) : INI
    end
