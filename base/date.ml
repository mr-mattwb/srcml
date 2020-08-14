open Unix
open Printf
open Pervasives

module YearSplit = EnvUnix.Hide(EnvUnix.Int(
    struct
        let context = ""
        let env = "YEAR_2_SPLIT"
        let default = 20
        let switch = ""
        let desc = "Point at which 2-digit years get converted to 1900 or 2000"
    end))

type elt = float
type time = elt

type year = int
type month = int
type day = int

type mon = 
    | Jan | Feb | Mar | Apr | May | Jun
    | Jul | Aug | Sep | Oct | Nov | Dec


type date = 
    {
        year : year;
        month : month;
        day : day
    }

external of_float : float -> elt = "%identity"
external to_float : elt -> float = "%identity"
let to_int64 dt = Int64.of_float dt 
let to_int64_str dt = Int64.to_string (to_int64 dt)

let to_psql_string ymd = 
    let lt = localtime ymd in
    sprintf "'%04d-%02d-%02d'::date" (1900+lt.tm_year) (1+lt.tm_mon) lt.tm_mday

let string_of_mon = function
    | Jan   -> "Jan"
    | Feb   -> "Feb"
    | Mar   -> "Mar"
    | Apr   -> "Apr"
    | May   -> "May"
    | Jun   -> "Jun"
    | Jul   -> "Jul"
    | Aug   -> "Aug"
    | Sep   -> "Sep"
    | Oct   -> "Oct"
    | Nov   -> "Nov"
    | Dec   -> "Dec"
let mon_of_string mon = 
    match String.uppercase mon with
    | "JAN" -> Jan
    | "FEB" -> Feb
    | "MAR" -> Mar
    | "APR" -> Apr
    | "MAY" -> May
    | "JUN" -> Jun
    | "JUL" -> Jul
    | "AUG" -> Aug
    | "SEP" -> Sep
    | "OCT" -> Oct
    | "NOV" -> Nov
    | "DEC" -> Dec
    | _ -> raise (Failure ("mon_of_string("^mon^")"))
let mon_of_substring ~ofs m =
    mon_of_string (String.sub m ofs 3)

let month_of_mon = function
    | Jan   -> 1
    | Feb   -> 2
    | Mar   -> 3
    | Apr   -> 4
    | May   -> 5
    | Jun   -> 6
    | Jul   -> 7
    | Aug   -> 8
    | Sep   -> 9
    | Oct   -> 10
    | Nov   -> 11
    | Dec   -> 12
let mon_of_month = function
    | 1  -> Jan
    | 2  -> Feb
    | 3  -> Mar
    | 4  -> Apr
    | 5  -> May
    | 6  -> Jun
    | 7  -> Jul
    | 8  -> Aug
    | 9  -> Sep
    | 10 -> Oct
    | 11 -> Nov
    | 12 -> Dec
    | mm -> raise (Failure (sprintf "mon_of_month(%d)" mm))
let int_of_day i = i
let day_of_int i = i
let day_of_string s = day_of_int (int_of_string s)
let string_of_day s = string_of_int s
let day_seconds = float_of_int (3600 * 24)
let seconds_of_days days = (float_of_int days) *. day_seconds
let int_of_month m = m
let int_of_day d = d

let days30 = [4; 6; 9; 11]
let days31 = [1; 2; 3; 5; 7; 8; 10; 12]

let year_of_string year = 
    try 
        let y = int_of_string year in
        if y < 0 then raise (Failure ("year_of_string("^year^")"))
        else if y < 100 then
            if y < (YearSplit.get()) then
                2000 + y
            else
                1900 + y
        else
            y
    with _ -> raise (Failure ("year_of_string("^year^")"))
let string_of_year year = string_of_int year

let year_of_substring ~ofs ds = 
    year_of_string (String.sub ofs 4 ds)

let yr_of_string yr = 
    try
        let yr' = int_of_string yr in
        let spl = YearSplit.get() in
        if yr' > 100 then yr'
        else if yr' < spl then 2000 + yr'
        else 1900 + yr'
    with _ -> raise (Failure ("yr_of_string("^yr^")")) 

let tm_of_date ymd = 
    let tm = {tm_year=(ymd.year-1900); tm_mon=(ymd.month-1); tm_mday=ymd.day;
              tm_hour=0; tm_min=0; tm_sec=0; tm_wday=0; tm_yday=0; tm_isdst=true}
    in
    snd (Unix.mktime tm)

let time_of_date ymd =
    let tm = {tm_year=(ymd.year-1900); tm_mon=(ymd.month-1); tm_mday=ymd.day;
              tm_hour=0; tm_min=0; tm_sec=0; tm_wday=0; tm_yday=0; tm_isdst=true}
    in
    fst (Unix.mktime tm)

let tm_of_time ut = Unix.localtime ut

let is_leap_year' year month day = 
    if (year mod 400) = 0 then true
    else if (year mod 100) = 0 then false
    else if (year mod 4) = 0 then true
    else false

let is_leap_year ymd = is_leap_year' ymd.year ymd.month ymd.day

let date_of_ints ~year ~month ~day = 
    let raise_failure () =
        let myd = sprintf "of_ints(year[%d], month[%d], day[%d])" year month day in
        raise (Failure myd)
    in
    if day <= 0 || day > 31 || year <= 0 || month <= 0 || month > 12 then
        raise_failure ()
    else if day > 30 && (month=4 || month=6 || month=9 || month=11) then
        raise_failure ()
    else if day > 28 && month=2 && not (is_leap_year' year month day) then
        raise_failure ()
    else
        {year=year; month=month; day=day}

let time_of_ints ~year ~month ~day = 
    time_of_date (date_of_ints ~year ~month ~day)

let to_date d = 
    let tm = Unix.localtime d in
    {year=(1900+tm.tm_year); month=(1+tm.tm_mon); day=tm.tm_mday}

let time_of_ddmmmyyyy dmy = 
    let slen = String.length dmy in
    if slen < 11 then raise (Failure ("of_dd_mmm_yyyy("^dmy^")"))
    else
        try
            let year = int_of_string (String.sub dmy 7 4) in
            let month = month_of_mon (mon_of_substring ~ofs:3 dmy) in
            let day = int_of_string (String.sub dmy 0 2) in
            time_of_ints ~year:year ~month:month ~day:day
        with _ ->
            raise (Failure (sprintf "of_dd_mm_yyyy(%s)" dmy))

let of_yyyy_mm_dd yyyy_mm_dd =
    let year = int_of_string (String.sub yyyy_mm_dd 0 4) in
    let month = int_of_string (String.sub yyyy_mm_dd 5 2) in
    let day =  int_of_string (String.sub yyyy_mm_dd 8 2) in
    let tm = {tm_year=year-1900; tm_mon=month-1; tm_mday=day; 
              tm_hour=0; tm_min=0; tm_sec=0; tm_wday=0; tm_yday=0; tm_isdst=true} in
    fst (Unix.mktime tm)

let zero = 0.0
let day_sub d days = d -. ((float days) *. day_seconds)
let now () = Unix.time()
let today () = 
    let tm = Unix.localtime (Unix.time()) in
    fst (mktime {tm with tm_hour=0; tm_min=0; tm_sec=0})
let yesterday () = day_sub (today()) 1

let past d = d < (now())
let future d = d > (now())
let future_day d = d > ((today()) +. day_seconds)
let is_today d = 
    let td = today () in
    (d >= td) && (d < (td +. day_seconds))

let eq a b = b >= a || b < (a +. day_seconds)
let lt a b = a < b
let le a b = a <= b
let (|<|) a b = lt a b
let (|<=|) a b = le a b
let lt_today a = a |<| (today())
let le_today a = a |<=| (today())

let add_days ~days d = d +. (seconds_of_days days)

let of_string s = of_yyyy_mm_dd s
let to_string s = 
    let lt = localtime s in
    sprintf "%04d-%02d-%02d" (1900 + lt.tm_year) (1 + lt.tm_mon) lt.tm_mday

let to_slash_string s = 
    let lt = localtime s in
    sprintf "%02d/%02d/%04d" (1 + lt.tm_mon) lt.tm_mday (1900 + lt.tm_year)

let of_dd_mmm_yyyy s = time_of_ddmmmyyyy s

let of_yyyymmdd s = 
    let is = int_of_string s in
    time_of_ints ~year:(is/10000) ~month:((is/100) mod 100) ~day:(is mod 100)

let date_sep_rxp = Str.regexp "[\\/-]"
let of_yyyy_mm_dd s = 
    try of_yyyymmdd (Str.global_replace date_sep_rxp "" s)
    with Failure f -> raise (Failure (f^": Srcml.Base.Utils.Date.of_yyyy_mm_dd"))
let of_mm_dd_yyyy =
    let rxp = Str.regexp "/" in
    let failure p = raise (Failure ("Srcml.Base.Utils.Date.of_mm_dd_yyyy:"^p)) in
    let convert p = 
        try int_of_string p 
        with e -> failure p
    in
    fun mmddyy -> 
        match Str.split rxp mmddyy with
        | mm :: dd :: yy :: [] -> 
            time_of_ints ~year:(convert yy) ~month:(convert mm) ~day:(convert dd)
        | _ -> 
            raise (Failure ("Srcml.Base.Utils.Date.of_mm_dd_yyyy:"^mmddyy))

let get_day d = (Unix.localtime d).tm_mday

module type DAY_VAR =
    sig
        open Cfg
        val env : env
        val default : day
        val switch : switch
        val desc : desc
    end
module type DAY = Cfg.ELT with type elt = day
module Day(E : DAY_VAR) = Cfg.Int(E)

module type SER = Ser.ELT with type elt = time
module type VAR = 
    sig
        val env : Cfg.env
        val default : elt
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
module Config(C : VAR) = Cfg.Make(Ser)(
    struct
        type elt = time
        include C
    end)

module Ini =
    struct
        module type INI = EnvCfg.ELT with type elt = time
        module type VAR = sig include EnvCfg.BVAR val default : time end
        module type VAR_STR = sig include EnvCfg.BVAR val default : string end
        module Make(V : VAR) = EnvUnix.Make(Ser)(
            struct
                type elt = time 
                include V
            end)
        module Str(V : VAR_STR) = Make(
            struct
                include V
                let default = Ser.of_str V.default
            end)
        module Zero(V : EnvCfg.BVAR) = Make(
            struct
                include V
                let default = zero
            end)
        module Now(V : EnvCfg.BVAR) = Make(struct include V let default = now() end)
    end


