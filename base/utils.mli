
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

val name : unit -> string
val name_join : string -> string -> string
val ( -|- ) : string -> string -> string

val upper : string -> string

val use : ('a -> 'b) -> ('b -> unit) -> ('b -> 'c) -> 'a -> 'c

val input_line : in_channel -> line option
val output_string : string -> out_channel -> unit
val output_line : ?nl:string -> string -> out_channel -> unit

val file_in : (in_channel -> 'a) -> file -> 'a
val file_out : (out_channel -> 'a) -> file -> 'a
val file_out_gen : open_flag list -> int -> (out_channel -> 'a) -> file -> 'a
val file_out_app : (out_channel -> 'a) -> file -> 'a
val file_in_bin : (in_channel -> 'a) -> file -> 'a
val temp_out : ?dir:string -> ?pfx:string -> ?sfx:string -> (file -> out_channel -> 'a) -> 'a

val chan_fold : (line -> 'a -> 'a) -> 'a -> in_channel -> 'a
val chan_iter : (line -> unit) -> in_channel -> unit
val chan_map : (line -> 'a) -> in_channel -> 'a list

val chan_lock_out : ?lt:Unix.lock_command -> ?pos:int -> 
    (out_channel -> 'a) -> out_channel -> 'a

val file_fold : (line -> 'a -> 'a) -> 'a -> file -> 'a
val file_iter : (line -> unit) -> file -> unit
val file_map : (line -> 'a) -> file -> 'a list

val file_lock_out : ?lt:Unix.lock_command -> (out_channel -> 'a) -> file -> 'a
val file_lock_app : ?lt:Unix.lock_command -> ?pos:int ->
    (out_channel -> 'a) -> file -> 'a

val proc_in : (in_channel -> 'a) -> cmd -> 'a
val proc_out : (out_channel -> 'a) -> cmd -> 'a
val proc : (in_channel * out_channel -> 'a) -> cmd -> 'a
val proc_iter : (line -> unit) -> cmd -> unit
val proc_fold : (line -> 'a -> 'a) -> 'a -> cmd -> 'a
val proc_map : (line -> 'a) -> cmd -> 'a list

val std_in : (in_channel -> 'a) -> file -> 'a
val std_out : (out_channel -> 'a) -> file -> 'a
val std_io : (in_channel -> out_channel -> 'a) -> file -> file -> 'a

val std_fold : (line -> 'a -> 'a) -> 'a -> file -> 'a
val std_iter : (line -> unit) -> file -> unit
val std_map : (line -> 'a) -> file -> 'a list

val output_string : string -> out_channel -> unit
val add_line : Buffer.t -> line -> unit

val put_file : file -> string -> unit
val put_file_line : file -> string -> unit
val app_file : file -> string -> unit
val add_file : ?b:Buffer.t -> file -> Buffer.t 
val get_file : ?b:Buffer.t -> file -> string
val get_file_line : file -> string option
val get_line_if_possible : file -> string option
val put_line_if_possible : file -> string -> file option

val unix_format : string -> string
val dos_format : string -> string
val strip_esc_ch : ?esc:char -> char -> string -> string
val xstrip_esc_ch : ?esc:char -> char -> string -> string
val rstrip_esc_ch : ?esc:char -> char -> string -> string
val bytes_rev : ?copy:bool -> bytes -> bytes
val string_rev : string -> string

module Dir :
  sig
    val use : (Unix.dir_handle -> 'a) -> dir -> 'a
    val readdir : Unix.dir_handle -> file option
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
    module Make(E : ELT) : S

    module type FILTER = 
      sig
        val test : file -> bool
      end
    module Filter(F : FILTER) : S
    module Hidden : S
    module Hiddens : S
      
  end

val error : exn -> string
val contents : Buffer.t -> string
val failf : ('a, unit, string, 'b) format4 -> 'a
val get_inet : string -> Unix.inet_addr

val copy : file -> file -> unit
val move : file -> file -> unit
val copy_to_dir : file -> dir -> file

val gen_file : string -> file

val spawn : ('a -> unit) -> 'a -> int 

val trim : string -> string
val substring : string -> int -> int -> string

type time = private int

val time_of_hhmm : string -> time
val hhmm_of_time : time -> string
val time_of_int : int -> time
val int_of_time : time -> int

val string_of_process_status : Unix.process_status -> string

val string_of_clist : string list -> string
val clist_of_string : string -> string list

val get_env : ?of_str:(string -> 'a) -> string -> 'a -> 'a

val env_pairs : unit -> (string * string) array
val env_keys : unit -> string array
val rex_getenv : ?simple:bool -> ?values:bool -> string -> string list
val rex_getenv_pairs : string -> (string * string) list

val env_sorted_keys : ?rev:bool -> unit -> string array
val env_sorted_key_list : ?rev:bool -> unit -> string list
val env_replace : string -> string
val env_command : string -> int

val chop_ext : file -> file
val find_file : ?env:string -> ?dir:string -> ?ext:string -> string -> string option

(*
%% - Replaced with %
%d - The directory name with the basename removed
%p - The full path
%b - The basenaem of the file
%e - The full name without the extension
%E - The basename without the extension
%x - The extension
%u - The date in the form YYYY-MM-DD
%t - The time with meridian in the form HHMMSSmm where mm is (AM/PM)
%T - The time in 24 hour format
%r - A random number
*)
val format_file : string -> string -> string
val rec_mkdir : ?perms:int -> string -> unit

val string_of_sockaddr : Unix.sockaddr -> string

val inet_default : unit -> Unix.inet_addr

val host_addr : Unix.inet_addr
val host_name : unit -> string

val timer : int -> ('a -> 'b) -> 'a -> 'b option
val itimer : float -> ('a -> 'b) -> 'a -> 'b option

val touch : file -> unit

val unquote : string -> string
val pcre_unquote : string -> string
val unquote2 : string -> string

val matched_tokens : string -> (string -> string list)
val unmatched_tokens : string -> (string -> string list)
val whitesp : string -> string list
val quoted_whitesp : string -> string list

val pidfile : file -> unit

val unansi_env_var : string         (* UNANSI_PATTERN *)
val unansi_pat : string             (* \r|\027(\\[[;0-9]*m|\\([A-Z) *)
val unansi_rex : Pcre.regexp
val unansi : ?pos:int -> string -> string
val plain : string -> string

val use_udp : ?dns:bool -> string * int -> (Unix.file_descr -> 'a -> 'b) -> 'a -> 'b

val service : ?fg:bool -> ?once:bool -> ('a -> unit) -> 'a -> unit

val sep_env : string
val sep : string
val sep_rex : Pcre.regexp
val list_split : ?sep:string -> string -> string list

val join_env : string
val join_def : string
val join : string
val list_join : ?sep:string -> string list -> string
val joiner : ?buf:Buffer.t -> ?sep:string -> to_str:('a -> string) -> 'a list -> string

val dir_files_exist : ?dirs:dir list -> file -> dir list
val files_exist : ?dirs:dir list -> file -> file list
val first_file : ?dirs:dir list -> file -> file option

val file_eq : file -> file -> bool
val file_cmp : file -> file -> int
module FileSet : Set.S with type elt = file

val dir_eq : dir -> dir -> bool
val dir_cmp : dir -> dir -> int
module DirSet :
    sig
        include Set.S with type elt = dir
        val elts : ?exist:bool -> t -> dir list
    end

val files_exist_set : ?dirs:dir list -> file -> file list
