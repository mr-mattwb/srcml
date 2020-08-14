
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

type ('a, 'b) fmt = ('a, unit, string, 'b) format4

module IdField = Int64

val modifier_pid : modifier -> modifier
val modifier : modifier

val null_id : id
val id_of_string : string -> id
val id_of_bytes : bytes -> id
val id_of_string_opt : string option -> id option
val id_of_bytes_opt : bytes option -> id option
val string_of_id : id -> string
val bytes_of_id : id -> bytes
val string_of_id_opt : id option -> string
val bytes_of_id_opt : id option -> bytes

val fmt_func_params : 'a list -> string

module Ini :
    sig
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

        module Id(V : VAR_ID) : ID
        module Modifier(V : VAR_MODIFIER) : MODIFIER
        module Host(V : VAR_HOST) : HOST
        module Dsn(V : VAR_DSN) : DSN
        module Uid(V : VAR_UID) : UID
        module Pwd(V : VAR_PWD) : PWD
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

module Id(IE : ID_VAR) : ID
module Modifier(ME : MODIFIER_VAR) : MODIFIER
module Dsn(E : DSN_VAR) : DSN
module Uid(E : UID_VAR) : UID
module Pwd(E : PWD_VAR) : PWD

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
module Select(Elt : ELT)(Row : ROW with type result = Elt.result) :
    SELECT with type db = Elt.db
            and type result = Row.result
            and type row = Row.row

module OneStr(Elt : ELT) : 
    ROW with type result = Elt.result
         and type row = string option
module OneInt(Elt : ELT) :
    ROW with type result = Elt.result
         and type row = int option

