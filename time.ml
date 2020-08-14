open Unix
open Printf
open Pervasives

type elt = int

type hour = int
type minute = int
type second = int
type hms = hour * minute * second

let zero = 0
let date_add dd dt =
    Date.of_float ((Date.to_float dd) +. (float_of_int dt))

let rxp = Str.regexp "[^0-9]"

external of_int : int -> elt = "%identity"
external to_int : elt -> int = "%identity"

let of_float f = int_of_float f
let to_float f = float_of_int f

let of_int64 i64 = Int64.to_int i64
let to_int64 i = Int64.of_int i

let to_string hms = sprintf "%02d:%02d:%02d" (hms / 3600) ((hms / 60) mod 60) (hms mod 60)
let to_psql_string hms = 
    sprintf "'%02d:%02d:%02d'::time" (hms / 3600) ((hms / 60) mod 60) (hms mod 60)

let of_ints (h, m, s) = (h * 3600) + (m * 60) + s
let of_strings (h, m, s) = 
    try
        of_ints (int_of_string h, int_of_string m, int_of_string s)
    with e ->
        raise (Failure (sprintf "Time.of_ints(hr[%s], min[%s], sec[%s])" h m s))

let time_of_ints ?(hr=0) ?(min=0) ?(sec=0) v = (of_ints (hr, min, sec)) + v
let time_of_strs ?(hr="00") ?(min="00") ?(sec="00") v = (of_strings (hr, min, sec)) + v

let sub = String.sub

let hours hms = hms / 3600
let minutes hms = (hms / 60) mod 60

let of_unsplit_string s = 
    match String.length s with
    | 2 -> time_of_strs ~hr:s 0
    | 4 -> time_of_strs ~hr:(sub s 0 2) ~min:(sub s 2 2) 0
    | 6 -> time_of_strs ~hr:(sub s 0 2) ~min:(sub s 2 2) ~sec:(sub s 4 2) 0
    | n when n < 6 -> raise (Failure ("Time["^s^"]"))
    | _ -> time_of_strs ~hr:(sub s 0 2) ~min:(sub s 2 2) ~sec:(sub s 4 2) 0

let of_string hms = 
    match Str.split rxp hms with
    | [] -> 0
    | h :: [] -> of_unsplit_string h
    | h :: m :: [] -> time_of_strs ~hr:h ~min:m 0
    | h :: m :: s :: _ -> time_of_strs ~hr:h ~min:m ~sec:s 0

let now () = 
    let tm = Unix.localtime (Unix.time()) in
    of_ints (tm.tm_hour, tm.tm_min, tm.tm_sec)

let time_sep_rxp = Str.regexp "[:]"
let of_hhmm s = 
    try 
        let is = int_of_string s in
        of_ints (is / 100, is mod 100, 0)
    with Failure f ->
        raise (Failure (f^": Srcml.Base.Time.of_hhmm"))

let of_hhmmss s = 
    try
        let is = int_of_string s in
        of_ints (is / 10000, (is / 100) mod 100, is mod 100)
    with Failure f ->
        raise (Failure (f^": Srcml.Base.Time.of_hhmmss"))

let to_hhmm hms = sprintf "%02d:%02d" (hours hms) (minutes hms)

type time = elt
module type SER = Ser.ELT with type elt = time
module type VAR = 
    sig
        val env : Cfg.env
        val default : time
        val switch : Cfg.switch
        val desc : Cfg.desc
    end
module type CFG = Cfg.ELT with type elt = time

module Ser = 
    struct
        type elt = time
        let of_str = of_string
        let to_str = to_string
    end

module Cfg(C : VAR) = Cfg.Make(Ser)(
    struct
        type elt = time
        include C
    end)

module Ini = 
    struct
        module type INI = 
            sig
                include EnvCfg.ELT with type elt = time
                val now : unit -> unit
            end
        module type VAR = sig include EnvCfg.BVAR val default : time end
        module type VAR_STR = sig include EnvCfg.BVAR val default : string end
        module Make(V : VAR) = 
            struct
                include EnvUnix.Make(Ser)(
                    struct
                        type elt = time
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
