open Unix
open Printf
open Pervasives

type line = string
type file = string
type dir = string
type cmd = string
type host = string
type port = int
type email = string
type sec = int
type msec = int32

module MSec = Int32

let name () = Filename.remove_extension (Filename.basename Sys.argv.(0))
let name_join pfx n = pfx ^ "::" ^ n
let ( -|- ) a b = name_join a b

let upper = String.uppercase_ascii

let use init close u arg = 
  let res = init arg in
  try
    let rc = u res in
    close res;
    rc
  with e ->
    (try close res with _ -> ());
    raise e

let input_line fin = 
  try 
      let line = Pervasives.input_line fin in
      let slen = String.length line in
      if slen > 0 && line.[slen-1] = '\r' then
          Some (String.sub line 0 (slen - 1))
      else
          Some line
  with End_of_file -> None
let output_string f fout = Pervasives.output_string fout f
let output_line ?(nl="\n") line fout =
    output_string line fout;
    output_string nl fout

let file_in fn f = use open_in close_in fn f
let file_out fn f = use open_out close_out fn f
let file_out_gen ol p fn f = use (open_out_gen ol p) close_out fn f
let file_out_app fn f = file_out_gen [Open_creat; Open_append] 0o666 fn f
let file_in_bin fn f = use open_in_bin close_in fn f
let temp_out ?dir ?(pfx="pfx") ?(sfx="sfx") fn =
    let fname = 
        match dir with
        | None -> Filename.temp_file pfx sfx 
        | Some temp_dir -> Filename.temp_file ~temp_dir pfx sfx
    in
    file_out (fn fname) fname

let rec chan_fold fn acc fin = 
    match input_line fin with
    | None -> acc
    | Some line -> chan_fold fn (fn line acc) fin
let chan_iter fn fin = 
    chan_fold (fun line () -> fn line) () fin
let chan_map fn fin = 
    chan_fold (fun line acc -> (fn line) :: acc) [] fin

let chan_lock_out ?(lt = F_LOCK) ?(pos = 0) fn fout = 
    let fd = descr_of_out_channel fout in
    lockf fd lt pos;
    try 
        let rc = fn fout in
        lockf fd F_ULOCK 0;
        rc
    with e ->
        (try lockf fd F_ULOCK 0 with _ -> ());
        raise e

let file_fold fn init fname =
  let rec folder acc fin =
    match input_line fin with
    | None -> acc
    | Some line -> folder (fn line acc) fin
  in
  file_in (folder init) fname
let file_iter fn f = file_fold (fun l () -> fn l) () f
let file_map fn f = 
  List.rev (file_fold (fun l ls -> (fn l) :: ls) [] f)

let file_lock_out ?(lt = F_LOCK) fn fname = 
    file_out (chan_lock_out ~lt fn) fname
let file_lock_app ?(lt = F_LOCK) ?(pos = 0) fn fname =
    file_out_app (chan_lock_out ~lt ~pos fn) fname

let close_process_in fin = ignore (Unix.close_process_in fin)
let close_process_out fout = ignore (Unix.close_process_out fout)
let close_process ff = ignore (Unix.close_process ff)
let proc_in fn proc = use open_process_in close_process_in fn proc
let proc_out fn proc = use open_process_out close_process_out fn proc
let proc fn proc = use open_process close_process fn proc
let proc_iter fn proc = proc_in (chan_iter fn) proc
let proc_fold fn acc proc = proc_in (chan_fold fn acc) proc
let proc_map fn proc = proc_in (chan_map fn) proc

let std_in fn = function
    | "" -> fn stdin
    | n -> file_in fn n
let std_out fn = function
    | "" -> fn stdout
    | n -> file_out fn n
let std_io fn inf outf = 
    std_out (std_in fn inf) outf

let std_fold fn init = function
    | "" -> chan_fold fn init stdin
    | n -> file_fold fn init n
let std_iter fn = function
    | "" -> chan_iter fn stdin
    | n -> file_iter fn n
let std_map fn = function
    | "" -> chan_map fn stdin
    | n -> file_map fn n
    
let add_line b line = 
    Buffer.add_string b line;
    Buffer.add_char b '\n'

let put_file fname str = file_out (output_string str) fname
let put_file_line fname str = file_out (output_line str) fname
let app_file fname str = file_out_app (output_string str) fname
let add_file ?(b=Buffer.create 1024) fname = 
    file_iter (add_line b) fname;
    b
let get_file ?(b=Buffer.create 1024) fname = 
    let b' = add_file ~b fname in
    Buffer.contents b'
let get_file_line fname = 
    file_in input_line fname

let get_line_if_possible fname = 
    try
        if Sys.file_exists fname then get_file_line fname
        else None
    with _ -> None
let put_line_if_possible fname v = 
    try 
        put_file_line fname v;
        Some fname
    with _ -> 
        None

let to_unix_rxp = Str.regexp "\r\n"
let to_dos_rxp = Str.regexp "\\([^\r]\\)\n"
let unix_format msg = Str.global_replace to_unix_rxp "\n" msg
let dos_format msg = Str.global_replace to_dos_rxp "\\1\r\n" msg
let strip_esc_ch ?(esc='\\') ch =
    let b = Buffer.create 1024 in
    let rec repl s pos = 
        if pos >= String.length s then Buffer.contents b
        else if (1+pos) < String.length s && (s.[pos] = '\\' && s.[pos+1] = ch) then
            (Buffer.add_char b s.[pos+1]; repl s (2+pos))
        else 
            (Buffer.add_char b s.[pos]; repl s (1+pos))
    in
    function s -> repl s 0
let xstrip_esc_ch ?(esc='\\') ch =  
    let sesc = String.make 1 esc in
    let sch = String.make 1 ch in
    let pat = sesc ^ sch in
    let rex = Pcre.regexp (Pcre.quote pat) in
    function s -> Pcre.replace ~rex ~templ:sch s
let rec rstrip_esc_ch ?(esc='\\') ch =
    let buf = Buffer.create 1024 in
    function s ->
        let rec repl pos = 
            if pos < 0 then Buffer.contents buf 
            else if pos > 0 && (s.[pos]=ch && s.[pos-1]=esc) then
                (Buffer.add_char buf s.[pos]; repl (pos-2))
            else 
                (Buffer.add_char buf s.[pos]; repl (pos-1))
        in
        string_rev (repl (String.length s - 1))

and bytes_rev ?(copy=false) sp = 
    let str = if copy then Bytes.copy sp else sp in
    let nul = Bytes.of_string "" in
    let rec swap_ch x y = 
        let t = Bytes.get str y in
        Bytes.set str y (Bytes.get str x);
        Bytes.set str x t
    and swap min max = 
        if min > max then str
        else (swap_ch min max; swap (min+1) (max-1))
    in
    match str with
    | c when c = nul -> c
    | c when Bytes.length c = 1 -> c
    | c when Bytes.length c = 2 -> swap_ch 0 1; c
    | c -> swap 0 (Bytes.length c - 1) 

and string_rev str = Bytes.to_string (bytes_rev (Bytes.of_string str))

module Dir =
  struct
    let old_use = use
    let use fn d = old_use opendir closedir fn d
    let readdir dh = 
      try Some (Unix.readdir dh)
      with End_of_file -> None

    module type ELT =
      sig
        val read : Unix.dir_handle -> file option
      end
    module type S =
      sig
        val fold : (file -> 'a -> 'a) -> 'a -> dir -> 'a
        val iter : (file -> unit) -> dir -> unit
        val map : (file -> 'a) -> dir -> 'a list
        val list : dir -> file list
      end
    module Make(E : ELT) =
      struct
        let fold fn init d =
          let rec folder acc dh = 
            match E.read dh with
            | Some f -> folder (fn f acc) dh
            | None -> acc
          in
          use (folder init) d
        let iter fn d = fold (fun f () -> fn f) () d
        let map fn d =
          List.rev (fold (fun f acc -> (fn f) :: acc) [] d)
        let list d = map (fun f -> f) d
      end
    
    module type FILTER = 
      sig
        val test : file -> bool
      end
    module Filter(F : FILTER) = Make(
      struct
        let rec read dh = 
          let v = readdir dh in
          match v with
          | None -> v
          | Some f when F.test f -> v
          | _ -> read dh
      end)
    module Hidden = Filter(
      struct
        let test = function
          | "." | ".." -> false
          | _ -> true
      end)
    module Hiddens = Filter(
      struct
        let test f = f.[0] <> '.'
      end)

    include Make(struct let read = readdir end)
  end

let error = function
  | Unix_error (e, fn, arg) ->
      sprintf "[%s][%s][%s]" (error_message e) fn arg
  | Sys_error s ->
      sprintf "SystemError[%s]" s
  | Failure f ->
      sprintf "Failure[%s]" f
  | e ->
      sprintf "[%s]" (Printexc.to_string e)

let contents b = 
  let s = Buffer.contents b in
  Buffer.reset b;
  s

let failf = 
    let throw msg = raise (Failure msg) in
    fun fmt -> ksprintf throw fmt

let rec get_inet v = 
    try (Unix.gethostbyname v).h_addr_list.(0)
    with Not_found -> ia_of_string v
and ia_of_string v = 
    try Unix.inet_addr_of_string v
    with Failure _ -> raise (Failure ("inet_addr_of_string:"^v))

let copy src dst = 
    let copy_out fin fout = 
        let str = Bytes.create 4096 in
        let rec loop () = 
            let len = input fin str 0 4096 in
            if len > 0 then
                (output fout str 0 len; loop())
        in
        loop ()
    in
    let copy_in fin = file_out (copy_out fin) dst in
    file_in_bin copy_in src
let move src dst = Unix.rename src dst
let copy_to_dir src dstd = 
    let dst = Filename.concat dstd (Filename.basename src) in
    copy src dst;
    dst

let gen_file pfx = sprintf "%s_%f" pfx (Unix.time())

let spawn (proc : 'a -> unit) (args : 'a) = 
    let run () =
        (try Unix.close Unix.stdout with _ -> ());
        (try Unix.close Unix.stderr with _ -> ());
        (try Unix.close Unix.stdin with _ -> ());
        ignore (proc args);
    and wait n = 
        match snd (waitpid [] n) with
        | WEXITED pid -> pid
        | WSIGNALED p -> p
        | WSTOPPED p -> p
    in
    match fork() with
    | 0 -> (match fork() with
            | 0 -> run (); 0
            | n -> exit n)
    | n -> wait n 

let left_trim_rxp = Str.regexp "^[ \t]*"
let right_trim_rxp = Str.regexp "[ \t\r]*$"
let trim str = 
    Str.global_replace left_trim_rxp ""
        (Str.global_replace right_trim_rxp "" str)
let rec substring str pos len = 
    let slen = String.length str in
    if pos > slen then ""
    else if (pos + len) > slen then 
        trim (String.sub str pos (slen-pos))
    else trim (String.sub str pos len)

type time = int
let sec_rxp = Str.regexp "[- :.]"
let secs_of_hh_mm_int hh mm = (hh * 3600) + (mm * 60)
let secs_of_hh_mm hh mm = 
    try
        secs_of_hh_mm_int (int_of_string hh) (int_of_string mm)
    with Failure _ -> 
        raise (Failure ("Seconds conversion ["^hh^":"^mm^"]"))
let secs_of_hh_mm_med' hh mm med = 
    let secs = secs_of_hh_mm hh mm in
    match upper med with
    | "AM" -> secs
    | "PM" -> secs + (12 * 3600)
    | _ -> secs
let secs_of_hh_mm_med s = 
    match Str.split sec_rxp s with
    | hh :: mm :: med :: _ -> secs_of_hh_mm_med' hh mm med
    | _ -> raise (Failure ("Invalid time ["^s^"]"))
let secs_of_hhcmm s c m =
    if m = 'a' || m = 'A' then
        secs_of_hh_mm_med' (String.sub s 0 2) (String.sub s 2 2) "AM"
    else if m = 'p' || m = 'P' then
        secs_of_hh_mm_med' (String.sub s 0 2) (String.sub s 2 2) "PM"
    else secs_of_hh_mm (String.sub s 0 2) (String.sub s 3 2)
let time_of_hhmm s = 
    let slen = String.length s in
    match slen with
    | 0 -> 0
    | 1 -> secs_of_hh_mm s "00"
    | 2 -> secs_of_hh_mm s "00"
    | 3 -> secs_of_hh_mm (String.sub s 0 1) (String.sub s 1 2)
    | 4 -> secs_of_hh_mm (String.sub s 0 2) (String.sub s 2 2)
    | 5 -> secs_of_hhcmm s s.[2] s.[4]
    | 6 -> secs_of_hh_mm_med' (String.sub s 0 2) (String.sub s 2 2)
            (String.sub s 4 2)
    | _ -> secs_of_hh_mm_med s
let hhmm_of_time s = 
    sprintf "%02d:%02d" (s / 3600) ((s / 60) mod 60)
let time_of_int s = s 
let int_of_time s = s

let string_of_process_status = 
    let to_string id rc = sprintf "%s:%d" id rc in
    function
    | WEXITED rc -> to_string "WEXITED" rc
    | WSIGNALED rc -> to_string "WSIGNALED" rc
    | WSTOPPED rc -> to_string "WSTOPPED" rc


let string_of_clist ls = 
    let buf = Buffer.create 1024 in
    let add b = Buffer.add_string buf b in
    let comma_add b = Buffer.add_char buf ','; add b in
    let fold () a = comma_add a in
    match ls with 
    | [] -> contents buf
    | x :: xs -> add x; List.fold_left fold () xs; contents buf

let clist_of_string = 
    let rxp = Str.regexp "[ \t\r\n]*,[ \t\r\n]*" in
    fun s -> Str.split_delim rxp s

(* get_env should be type-safe, but the compiler can't handle it *)
let id x = x
let get_env ?(of_str=(Obj.magic id)) (env : string) (def : 'a) = 
    try of_str (Unix.getenv env)
    with _ -> def

let getenv env def = 
    try Unix.getenv env
    with Not_found -> def
let env_list () = Array.to_list (Unix.environment())
let key_pair = 
    let rex = Pcre.regexp "=" in
    fun kv ->
        match Pcre.split ~rex ~max:2 kv with
        | [] -> "", ""
        | k :: [] -> k, ""
        | k :: v :: _ -> k, v

let env_pairs () = Array.map key_pair (Unix.environment())
let env_keys () =
    let get_key kv = fst (key_pair kv) in
    Array.map get_key (Unix.environment())
let rex_getenv_simple pat = 
    let rex = Pcre.regexp pat in
    let matches kv = Pcre.pmatch ~rex kv in
    List.filter matches (Array.to_list (environment()))
let rex_getenv_keys pat = 
    let rex = Pcre.regexp pat in
    let matches acc kv = 
        let k = fst (key_pair kv) in 
        if Pcre.pmatch ~rex k then k :: acc
        else acc
    in Array.fold_left matches [] (environment())
let rex_getenv_values pat = 
    let rex = Pcre.regexp pat in
    let matches acc kv = 
        let k, v = key_pair kv in
        if Pcre.pmatch ~rex k then v :: acc
        else acc
    in Array.fold_left matches [] (environment())
let rex_getenv ?(simple=false) ?(values=false) pat = 
    if simple then rex_getenv_simple pat
    else if values then rex_getenv_values pat
    else rex_getenv_keys pat
let rex_getenv_pairs pat = 
    let rex = Pcre.regexp pat in
    let matches acc kv = 
        let k, v = key_pair kv in
        if Pcre.pmatch ~rex k then (k, v) :: acc
        else acc
    in Array.fold_left matches [] (environment())

let env_sorted_keys ?(rev=true) () = 
    let sorter =
        if rev then  (fun a b -> compare (String.length b) (String.length a))
        else (fun a b -> compare (String.length a) (String.length b)) 
    in
    let keys = env_keys () in
    Array.sort sorter keys;
    keys
let env_sorted_key_list ?(rev=true) () = Array.to_list (env_sorted_keys ~rev ())
let env_sorted_key_values ?(rev=true) ()  =
    Array.to_list (Array.map (fun k -> k, (Unix.getenv k)) (env_sorted_keys ~rev ()))
let env_replace msg =
    let kvass = env_sorted_key_values ~rev:true () in
    let accum m (k, v) = 
        let rex = Pcre.regexp (Pcre.quote ("$"^k)) in
        Pcre.qreplace ~rex ~templ:v m
    in
    List.fold_left accum msg kvass
let env_command cmd = Sys.command (env_replace cmd)

let chop_ext f = 
    try Filename.chop_extension f 
    with 
    | Invalid_argument _
    | Not_found -> f

open Printf
let rec find_file ?(env="FILE") ?(dir="./") ?(ext="") f =
    let add_suffix f =
        if Filename.check_suffix f ext then f
        else if String.length ext > 1 && ext.[0]='.' then f^ext
        else f^"."^ext
    in 
    let def = if f="" then "default" else f in
    let default = add_suffix def in          (* Start with default *)
    let fname =                                 
        try add_suffix (Unix.getenv env)       (* if env exists, use *)
        with Not_found -> default                 (* else use default *)
    in
    let rec try_home_file () = 
        try
            let fn = Filename.concat (Unix.getenv "HOME") fname in
            if Sys.file_exists fn then Some fn
            else try_dot_dir ()
        with Not_found ->
            try_dot_dir ()
    and try_dot_dir () = 
        let fn = Filename.concat dir fname in
        if Sys.file_exists fn then Some fn
        else try_home_dot_dir fn
    and try_home_dot_dir fn = 
        try
            let fn' = Filename.concat (Unix.getenv "HOME") fn in
            if Sys.file_exists fn' then Some fn'
            else try_path ()
        with Not_found ->
            try_path ()
    and try_path () = 
        try
            let rxp = Str.regexp ":" in
            let exists d = Sys.file_exists (Filename.concat d fname) in
            let d = List.find exists (Str.split_delim rxp (Unix.getenv "PATH")) in
            Some (Filename.concat d fname)
        with Not_found -> 
            None 
    in
    if Sys.file_exists fname then Some fname
    else if Filename.basename fname = fname then try_home_file ()
    else None

let fmt_extension f = 
    let buf = Buffer.create (String.length f) in
    let rec loop idx = 
        if idx <= 0 then ()
        else if f.[idx] = '.' then ()
        else (loop (idx-1); Buffer.add_char buf f.[idx])
    in
    loop (String.length f - 1);
    Buffer.contents buf
let fmt_yyyy_mm_dd () =
    let tm = Unix.localtime (Unix.time ()) in
    sprintf "%04d-%02d-%02d" (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday
let fmt_time_mer () =
    let tm = Unix.localtime (Unix.time()) in
    sprintf "%02d%02d%02d%s" (tm.tm_hour mod 12) tm.tm_min tm.tm_sec
        (if tm.tm_hour >= 12 then "PM" else "AM")
let fmt_time () = 
    let tm = Unix.localtime (Unix.time()) in
    sprintf "%02d%02d%02d" tm.tm_hour tm.tm_min tm.tm_sec
let remove_patterns = 
    let rxp = Str.regexp "'[^']+'" in
    fun p -> Str.global_replace rxp "" p
let random_number msg = 
    let rxp = Str.regexp "^.*%\\([0-9]+\\)r.*$" in
    let m = Str.global_replace rxp "\\1" msg in
    let mi = try if m = "" then 1 else int_of_string m with Failure _ -> 1 in
    Random.init mi;
    string_of_int (Random.int mi)
let format_file fn target =
    let forms = [ "%", Str.regexp "%%";
                  Filename.dirname fn, Str.regexp "%d";
                  fn, Str.regexp "%p";
                  Filename.basename fn, Str.regexp "%b";
                  chop_ext fn, Str.regexp "%e";
                  chop_ext (Filename.basename fn), Str.regexp "%E";
                  fmt_extension fn, Str.regexp "%x";
                  random_number target, Str.regexp "%[0-9]+r";
                  fmt_yyyy_mm_dd (), Str.regexp "%u";
                  fmt_time_mer (), Str.regexp "%t";
                  fmt_time (), Str.regexp "%T" ]
    in
    let aux acc (repl, pat) = Str.global_replace pat repl acc in
    List.fold_left aux target forms

let rec_mkdir ?(perms=0o755) path = 
    let rec mkdir p = 
        try 
            if Sys.file_exists p then ()
            else (mkdir (Filename.dirname p); Unix.mkdir p perms)
        with 
        | Unix_error (EEXIST, "mkdir", _) -> ()
        | Unix_error (ENOENT, "mkdir", _) -> ()
    in
    mkdir (Filename.dirname path)

let rec string_of_sockaddr = function
    | ADDR_UNIX f -> "ADDR_UNIX("^f^")"
    | ADDR_INET(ia, p) -> sprintf "INET_ADDR(%s, %d)" (inet_addr_string ia) p
and inet_addr_string ia = 
    try (gethostbyaddr ia).h_name
    with Not_found -> string_of_inet_addr ia

let inet_default () = 
    try (gethostbyname (gethostname())).h_addr_list.(0)
    with e -> inet_addr_of_string "127.0.0.1"

let host_addr = inet_addr_of_string "127.0.1.1"
let host_name () = 
    try (gethostbyaddr host_addr).h_name
    with Not_found -> (gethostbyname "localhost").h_name

exception Timeout

let raise_timeout _ = raise Timeout

let timer secs fn args = 
    let osigh = Sys.signal Sys.sigalrm (Sys.Signal_handle raise_timeout) in
    let oalrm = alarm secs in
    let reset () =
        ignore (alarm oalrm);
        Sys.set_signal Sys.sigalrm osigh
    in
    try
        let rc = fn args in
        reset ();
        Some rc
    with 
    | Timeout -> (try reset() with _ -> ()); None
    | e -> (try reset () with _ -> ()); raise e
    

let itimer msecs fn args = 
    let rit = {it_interval=0.0; it_value=msecs} in 
    let oshnd = Sys.signal Sys.sigalrm (Sys.Signal_handle raise_timeout) in
    let oit = setitimer ITIMER_REAL rit in
    let reset ()  = 
        ignore (setitimer ITIMER_REAL oit);
        Sys.set_signal Sys.sigalrm oshnd
    in
    try
        let rc = fn args in
        reset();
        Some rc
    with 
    | Timeout -> (try reset() with _ -> ()); None
    | e -> (try reset () with _ -> ()); raise e

let touch f = close_in (open_in f)

let unquote str = 
    let len = String.length str in
    if len = 0 || len = 1 || str.[0] <> '"' then str
    else if str.[0]='"' && str.[len-1]='"' then String.sub str 1 (len-2)
    else str
let pcre_unquote = 
    let rex = Pcre.regexp ~flags:[`DOTALL;`MULTILINE] "^\"(([^\"]|\\\\\")*)\"$" in
    let itempl = Pcre.subst "$1" in
    fun str -> Pcre.replace ~pat:"\\\\\"" ~templ:"\"" (Pcre.replace ~rex ~itempl str)

let unquote2 str = 
    let buf = Buffer.create (String.length str) in
    let strm = Stream.from (function x -> if x >= String.length str then None else Some (String.get str x)) in
    let rec noquote = function
        | '"' -> quoted (Stream.next strm)
        | n -> Buffer.add_char buf n; noquote (Stream.next strm)
    and quoted = function
        | '"' -> noquote (Stream.next strm)
        | '\\' -> escaped (Stream.next strm)
        | n -> Buffer.add_char buf n; quoted (Stream.next strm)
    and escaped =  function
        | '"' -> Buffer.add_char buf '"'; quoted (Stream.next strm)
        | '\\' -> Buffer.add_char buf '\\'; quoted '\\'
        | n -> Buffer.add_char buf '\\'; Buffer.add_char buf n; quoted (Stream.next strm)
    in
    try
        noquote (Stream.next strm);
        Buffer.contents buf
    with Stream.Failure ->
        Buffer.contents buf


let matched_tokens pat =
    let rex = Pcre.regexp ~flags:[`DOTALL;`MULTILINE] pat in
    let next ss = Pcre.next_match ~rex ss in
    let extract ss = (Pcre.get_substrings ss).(0) in
    let get ss = try Some (next ss) with Not_found -> None in
    let rec loop acc ss = 
        match get ss with
        | None -> List.rev acc
        | Some ss' -> loop (extract ss' :: acc) ss'
    in
    fun str -> 
        try 
            let ss = Pcre.exec ~rex str in
            loop [extract ss] ss
        with Not_found -> 
            []

let unmatched_tokens pat = 
    let rex = Pcre.regexp ~flags:[`DOTALL;`MULTILINE] pat in
    fun s -> Pcre.split ~rex s

let whitesp = 
    let split = unmatched_tokens "[ \r\n\t]+" in
    fun str -> split str
let quoted_whitesp = 
    let split = matched_tokens "[^ \r\n\t\"]+|\"[^\"]*\"" in 
    fun str -> List.map unquote (split str)

let pidfile pf = put_file pf (string_of_int (getpid()))

let unansi_env_var = "UNANSI_PATTERN"
let unansi_pat =
    try Unix.getenv unansi_env_var
    with Not_found -> "\r|\027(\\[[;0-9]*m|\\([A-Z])"
let unansi_rex = Pcre.regexp unansi_pat
let unansi ?(pos=0) str = 
    Pcre.replace ~rex:unansi_rex ~templ:"" (String.sub str pos (String.length str - pos))
        
let plain txt = 
    let buf = Buffer.create (String.length txt) in
    let add_char = Buffer.add_char in
    let add_str = Buffer.add_string in
    let add ch = 
        match ch with
        | '\n' -> add_char buf '\\'; add_char buf 'n'
        | '\r' -> add_char buf '\\'; add_char buf 'r'
        | '\027' -> add_char buf '\\'; add_str buf "027"
        | n -> add_char buf n
    in
    String.iter add txt;
    Buffer.contents buf

let use_udp ?(dns=true) (h, p) fn args = 
    let inet = 
        try 
            if dns then (gethostbyname h).h_addr_list.(0)
            else inet_addr_of_string h
        with Not_found -> inet_addr_of_string h
    in
    let fd = socket PF_INET SOCK_DGRAM 0 in
    let sa = ADDR_INET(inet, p) in
    try
        bind fd sa;
        let rc = fn fd args in
        close fd;
        rc
    with e ->
        (try close fd with _ -> ());
        raise e

let service ?(fg=false) ?(once=false) fn args =
    let rec main () = 
        if once then fn args
        else loop ()
    and loop () = 
        fn args;
        loop ()
    in
    if fg then main ()
    else ignore (spawn main ())

let sep_env = "SEP"
let sep = try Unix.getenv sep_env with Not_found -> "\\s+"
let sep_rex = Pcre.regexp sep
let list_split ?(sep=sep) =
    let rex = Pcre.regexp sep in
    fun str -> Pcre.split ~rex str

let join_env = "JOIN"
let join_def = " "
let join = try Unix.getenv join_env with Not_found -> join_def
let list_join ?(sep=join) ls = 
    let buf = Buffer.create 1024 in
    let rec items x xs = add x; List.iter join_add xs
    and add x = Buffer.add_string buf x
    and join_add x = Buffer.add_string buf sep; add x in
    match ls with
    | [] -> ""
    | x :: [] -> x
    | x :: xs -> items x xs; Buffer.contents buf
let joiner ?(buf=Buffer.create 1024) ?(sep=join) ~to_str ls = 
    let add s = Buffer.add_string buf s in
    let buffer_item x = add (to_str x) in
    let next x = add sep; buffer_item x in
    match ls with
    | [] -> ""
    | x :: [] -> to_str x
    | x :: xs -> 
        buffer_item x;
        List.iter next xs;
        Buffer.contents buf

let dir_files_exist ?(dirs = ["."]) fn =
    let exists d = Sys.file_exists (Filename.concat d fn) in
    List.filter exists dirs

let files_exist ?(dirs = ["."]) fn = 
    let accum acc d =
        let fp = Filename.concat d fn in
        if Sys.file_exists fp then fp :: acc
        else acc
    in
    List.fold_left accum [] dirs 

let first_file ?(dirs = ["."]) fn = 
    let rec loop = function
        | [] -> None
        | d :: ds -> next d ds
    and next d ds = 
        let fp = Filename.concat d fn in
        if Sys.file_exists fp then Some fp
        else loop ds
    in
    loop dirs

let file_eq f1 f2 = 
    try (Unix.stat f1) = (Unix.stat f2)
    with Unix_error(ENOENT, _, _) -> f1 = f2
let file_cmp f1 f2 = 
    try compare (Unix.stat f1) (Unix.stat f2)
    with Unix_error(ENOENT, _, _) -> String.compare f1 f2
module FileSet = Set.Make(
    struct
        type t = file
        let compare = file_cmp
    end)

let dir_eq d1 d2 = 
    try  
        let s1 = stat d1 and s2 = stat d2 in
        s1 = s2 && s1.st_kind = S_DIR && s2.st_kind = S_DIR
    with Unix_error(ENOENT, _, _) -> d1 = d2
let rec dir_cmp d1 d2 = 
    try compare (stat d1) (stat d2)
    with Unix_error(ENOENT, _, _) -> dir_str_cmp d1 d2
and dir_str_cmp d1 d2 = 
    if String.length d1 > 0 && d1.[0] = '/' then 1
    else if String.length d2 > 0 && d2.[0] = '/' then -1
    else String.compare d1 d2
module DirSet = 
    struct
        include Set.Make(
            struct
                type t = dir
                let compare = dir_cmp
            end)
        let elts ?(exist=false) set =
            let dir_exists d = Sys.file_exists d && Sys.is_directory d in
            match elements set with
            | [] -> []
            | ls -> if exist then List.filter dir_exists ls else ls
    end

let files_exist_set ?(dirs = ["."]) fn = 
    let accum fset d = 
        let fp = Filename.concat d fn in
        if Sys.file_exists fp then FileSet.add fp fset
        else fset
    in
    FileSet.elements (List.fold_left accum FileSet.empty dirs)

