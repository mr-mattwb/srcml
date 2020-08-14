open Unix
open Printf
open Pervasives

type elt = float
type datetime = elt

let to_float dtts = dtts
let of_float dtts = dtts
let to_int64 dt = Int64.of_float dt

let zero = 0.0

let now () = Unix.time ()
let of_tm tm = fst (Unix.mktime tm)
let of_date d = Date.to_float d
let to_date d = Date.of_float d
let add_time d tm = d +. (Time.to_float tm)
let of_time tm = 
    let n = Date.today() in
    Date.to_float n +. Time.to_float tm

let add_days ~days d = d +. (Date.seconds_of_days days)

let rec of_string dt = 
    let md = Date.Ser.of_str dt in
    (Date.to_float md) +. (Time.to_float (time_substr dt))
and time_substr dt = 
    try
        let spc = String.index dt ' ' in
        Time.Ser.of_str (String.sub dt (spc+1) (String.length dt - spc - 1))
    with 
    | Not_found -> Time.zero
    | Invalid_argument _ -> Time.zero

let of_yyyymmdd s = of_date (Date.of_yyyymmdd s)
let of_yyyy_mm_dd s = of_date (Date.of_yyyy_mm_dd s)

let of_ddmmmyyyy d = Date.to_float (Date.of_dd_mmm_yyyy d)

let rec of_ddmmmyyyy_hhmm str = 
    let len = String.length str in
    let tm = String.sub str (len - 4) 4 in
    let dt = String.sub str 0 (len - 4 - 1) in
    (of_ddmmmyyyy dt) +. (Time.to_float (Time.of_hhmm tm))

module type SER = Ser.ELT with type elt = datetime
module type VAR =
    sig
        val env : Cfg.env
        val default : elt
        val switch : Cfg.switch
        val desc : Cfg.desc
    end
module type CFG = Cfg.ELT with type elt = datetime

module Ser = 
    struct
        type elt = datetime
        let to_str ts = 
            let lt = Unix.localtime ts in
            sprintf "%04d-%02d-%02d %02d:%02d:%02d"
                (1900+lt.tm_year) (1+lt.tm_mon) lt.tm_mday
                lt.tm_hour lt.tm_min lt.tm_sec
        let of_str s = of_string s
    end
module OCfg = Cfg
module Cfg(C : VAR) = OCfg.Make(Ser)(
    struct
        type elt = datetime
        include C
    end)

module Ini = 
    struct
        module type INI = 
            sig
                include EnvCfg.ELT with type elt = datetime
                val now : unit -> unit
            end
        module type VAR = sig include EnvCfg.BVAR val default : datetime end
        module type VAR_STR = sig include EnvCfg.BVAR val default : string end
        module Make(V : VAR) = 
            struct
                include EnvUnix.Make(Ser)(
                    struct
                        type elt = datetime
                        include V
                    end)
                let now () = put (now()) 
            end
        module Str(V : VAR_STR) = Make(
            struct
                include V
                let default = Ser.of_str V.default
            end)
        module Zero(V : EnvCfg.BVAR) = Make(struct include V let default = zero end)
        module Now(V : EnvCfg.BVAR) = Make(struct include V let default = now() end)
    end

