open Unix
open Printf
open Pervasives

open Utils

exception More_results
exception Column_out_of_range
exception Invalid_column of string
exception Row_error of string * string
exception Required of string

type id = int64

type modifier = string

type host = string
type dsn = string
type uid = string
type pwd = string

type name = string
type row = int
type column = int
type 'a data = 'a option
type query = string 
type param = bytes
type field = string data
type field_row = bytes data array

type ('a, 'b) fmt = ('a, unit, string, 'b) format4

module IdField = Int64

let modifier_pid m = sprintf "%s:%08d" m (Unix.getpid())
let modifier = modifier_pid (Filename.basename Sys.argv.(0))

let null_id = 0L

let id_of_string s = 
    let cnv v = 
        try Int64.of_string v
        with _ -> null_id
    in
    match s with
    | "" -> null_id
    | v -> cnv v
let id_of_bytes s = id_of_string (Bytes.to_string s)

let id_of_string_opt = function
    | None -> None
    | Some id -> Some (id_of_string id)
let id_of_bytes_opt = function
    | None -> None
    | Some id -> Some (id_of_bytes id)
let string_of_id id = Int64.to_string id
let bytes_of_id id = Bytes.of_string (Int64.to_string id)
let string_of_id_opt = function
    | None -> ""
    | Some v -> Int64.to_string v
let bytes_of_id_opt = function
    | None -> Bytes.of_string ""
    | Some v -> Bytes.of_string (Int64.to_string v)

let fmt_func_params ls = 
    let buf = Buffer.create 1024 in
    let add_arg _ = Buffer.add_char buf '?' in
    let add _ = Buffer.add_char buf ','; add_arg () in
    let additer = function
        | [] -> ()
        | x :: [] -> add_arg x
        | x :: xs -> add_arg x; List.iter add xs
    in
    Buffer.add_char buf '(';
    additer ls;
    Buffer.add_char buf ')';
    Buffer.contents buf

module Ini =
    struct
        module type ID = EnvCfg.ELT with type elt = id 
        module type MODIFIER = EnvCfg.ELT with type elt = modifier
        module type HOST = EnvCfg.ELT with type elt = host
        module type DSN = EnvCfg.ELT with type elt = dsn
        module type UID = EnvCfg.ELT with type elt = uid
        module type PWD = EnvCfg.ELT with type elt = pwd

        module type VAR_ID = sig include EnvCfg.BVAR val default : id end
        module type VAR_MODIFIER = sig include EnvCfg.BVAR val default : modifier end
        module type VAR_HOST = sig include EnvCfg.BVAR val default : host end
        module type VAR_DSN = sig include EnvCfg.BVAR val default : dsn end
        module type VAR_UID = sig include EnvCfg.BVAR val default : uid end
        module type VAR_PWD = sig include EnvCfg.BVAR val default : pwd end

        open EnvUnix
        module Id(V : VAR_ID) = Int64(V)
        module Modifier(V : VAR_MODIFIER) = Str(V)
        module Host(V : VAR_HOST) = Str(V)
        module Dsn(V : VAR_DSN) = Str(V)
        module Uid(V : VAR_UID) = Str(V)
        module Pwd(V : VAR_PWD) = Str(V)
    end

module type ID = Cfg.ELT with type elt = id
module type MODIFIER = Cfg.ELT with type elt = modifier
module type HOST = Cfg.ELT with type elt = host
module type DSN = Cfg.ELT with type elt = dsn
module type UID = Cfg.ELT with type elt = uid
module type PWD = Cfg.ELT with type elt = pwd

module type ID_VAR = 
    sig
        open Cfg
        val env : env
        val default : id
        val switch : switch
        val desc : desc
    end
module type MODIFIER_VAR =
    sig
        open Cfg
        val env : env
        val default : modifier
        val switch : switch
        val desc : desc
    end
module type HOST_VAR = 
    sig
        include Cfg.BVAR
        val default : host
    end
module type DSN_VAR =
    sig
        open Cfg
        val env : env
        val default : dsn
        val switch : switch
        val desc : desc
    end
module type UID_VAR =
    sig
        open Cfg
        val env : env
        val default : uid
        val switch : switch
        val desc : desc
    end
module type PWD_VAR =
    sig
        open Cfg
        val env : env
        val default : pwd
        val switch : switch
        val desc : desc
    end

module Id(IE : ID_VAR) = Cfg.Int64(IE)
module Modifier(ME : MODIFIER_VAR) = Cfg.Str(ME)
module Host(E : HOST_VAR) = Cfg.Str(E)
module Dsn(E : DSN_VAR) = Cfg.Str(E)
module Uid(E : UID_VAR) = Cfg.Str(E)
module Pwd(E : PWD_VAR) = Cfg.Str(E)

module type ELT =
  sig
    type args 
    type db
    type result
    val connect : args -> db
    val disconnect : db -> unit
    val use : (db -> 'a) -> args -> 'a
    val direct_s : db -> string -> field list -> unit
    val direct : db -> field list -> ('a, unit) fmt -> 'a
    val exec_s : db -> string -> field list -> result
    val exec : db -> field list -> ('a, result) fmt -> 'a
    val free : result -> unit
    val rows : result -> int
    val columns : result -> int
    val currow : result -> int
    val use_query_s : (result -> 'a) -> db -> string -> field list -> 'a
    val use_query : (result -> 'a) -> db -> field list -> ('b, 'a) fmt -> 'b
    val fetch : result -> unit
    val get : result -> column -> bytes option
    val gets : result -> column -> string option
    val get_bytes : result -> column -> bytes
    val get_str : result -> column -> string
    val get_name : result -> name -> bytes option
    val get_name_str : result -> name -> string option
    val get_row_list : result -> bytes option list
    val get_row_array : result -> bytes option array
    val get_row_list_str : result -> string option list
    val get_row_array_str : result -> string option array
    val get_str_list : result -> string list
    val get_bytes_list : result -> bytes list
    val get_str_array : result -> string array
    val get_bytes_array : result -> bytes array
  end

module type ROW =
    sig
        type result
        type row
        val get : result -> row
    end

module type SELECT =
    sig
        type db
        include ROW
        val next : result -> row option
        val fold : (row -> 'a -> 'a) -> 'a -> db -> query -> field list -> 'a
        val map : (row -> 'a) -> db -> query -> field list -> 'a list
        val iter : (row -> unit) -> db -> query -> field list -> unit
        val stream : db -> query -> field list -> row Stream.t
        val row : db -> query -> field list -> row option

        val foldi : (int -> row -> 'a -> 'a) -> 'a -> db -> query -> field list -> 'a
        val mapi : (int -> row -> 'a) -> db -> query -> field list -> 'a list
        val iteri : (int -> row -> unit) -> db -> query -> field list -> unit

        val iterf : ?fields:field list -> (row -> unit) -> db -> ('a, unit) fmt -> 'a
        val mapf : ?fields:field list -> (row -> 'a) -> db -> ('b, 'a list) fmt -> 'b
        val foldf : ?fields:field list -> (row -> 'a -> 'a) -> 'a -> db -> ('b, 'a) fmt -> 'b
        val streamf : ?fields:field list -> db -> ('a, row Stream.t) fmt -> 'a
    end
module Select(Elt : ELT)(Row : ROW with type result = Elt.result) =
    struct
        type db = Elt.db
        include Row
        let next r = 
            try 
                Elt.fetch r; 
                Some (Row.get r)
            with 
            | End_of_file -> None
            | _ -> None
        let fold fn init =
            let rec loop acc r = 
                match next r with
                | None -> acc
                | Some g -> loop (fn g acc) r
            in
            Elt.use_query_s (loop init)
        let map fn =
            let rec loop r = 
                match next r with
                | None -> []
                | Some v -> (fn v) :: (loop r)
            in
            Elt.use_query_s loop
        let iter fn =
            let rec loop r = 
                match next r with
                | None -> ()
                | Some v -> fn v; loop r
            in
            Elt.use_query_s loop
        let stream db q p = 
            let r = Elt.exec_s db q p in
            at_exit (fun () -> Elt.free r);
            let aux ind = next r in
            Stream.from aux

        let foldi fn init = 
            let rec loop pos acc r = 
                match next r with
                | None -> acc
                | Some v -> loop (1+pos) (fn pos v acc) r
            in
            Elt.use_query_s (loop 1 init)
        let mapi fn = 
            let rec loop pos r = 
                match next r with
                | None -> []
                | Some v ->  (fn pos v) :: (loop (pos+1) r)
            in
            Elt.use_query_s (loop 1)
        let iteri fn = 
            let rec loop pos r = 
                match next r with
                | None -> ()
                | Some v -> fn pos v; loop (pos+1) r
            in
            Elt.use_query_s (loop 1)

        let row = Elt.use_query_s next

        let iterf ?(fields=[]) fn db fmt = let query q = iter fn db q fields in ksprintf query fmt
        let mapf ?(fields=[]) fn db fmt = let query q = map fn db q fields in ksprintf query fmt
        let foldf ?(fields=[]) fn init db fmt = 
            let query q = fold fn init db q fields in 
            ksprintf query fmt
        let streamf ?(fields=[]) db fmt = 
            let query q = stream db q fields in 
            ksprintf query fmt
    end

module OneStr(Elt : ELT) = 
    struct
        type result = Elt.result
        type row = string data
        let get r = Elt.gets r 0
    end
module OneInt(Elt : ELT) = 
    struct
        type result = Elt.result
        type row = int data
        let get r = 
            match Elt.gets r 0 with
            | None -> None
            | Some v ->
                let v' = 
                    try int_of_string v 
                    with Failure _ -> 
                        raise (Failure (sprintf "OneInt:  Cannot convert [%s] to int" v))
                in Some v'
    end

module type NAMED =
    sig
        val name : name
    end

module Named(Elt : ELT)(N : NAMED) = Select(Elt)(
    struct
        type result = Elt.result
        type row = string data
        let get r = Elt.get_name_str r N.name
    end)

