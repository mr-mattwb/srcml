open Unix
open Printf
open Pervasives

open IniBase
open OSer
open OVar

(* Config *)

module Specs = Set.Make(
    struct
        type t = spec
        let compare (a1, _, _) (a2, _, _) = String.compare a1 a2
    end)

class type config_elt = 
    object
        method add_ini_file : Utils.file -> unit
        method try_ini_file : Utils.file -> unit
        method add_unix_env : unit -> unit
    end

class type access_elt = 
    object
        inherit config_elt
        method get : evar_elt -> value option
        method put : evar_elt -> value -> unit
        method find : evar_elt -> value
        method rem : evar_elt -> unit
        method add : evar_elt -> value -> unit
        method update : bvar_elt -> value -> unit
    end

class type out_elt = 
    object
        inherit access_elt
        method buffer : Buffer.t -> unit
        method to_string : unit -> string
        method output : out_channel -> unit
        method print : unit -> unit
    end

class type cfg_elt = 
    object
        inherit out_elt
        method register : spec -> unit
        method param_list : unit -> spec list
    end

(* config vars *)

class type ['a] env_elt = 
    object
        method config : cfg_elt
        method env : string
        method var : 'a OVar.elt
        method ser : 'a OSer.elt
        method desc : desc
        method get : unit -> 'a
        method put : 'a -> unit
        method arg : spec
    end

class type ['a] elt = 
    object
        inherit ['a] env_elt
    end

class type flag_elt = 
   object
        inherit [bool] elt
    end

class type dir_elt = 
    object
        inherit [Utils.dir] elt
        method dir : Utils.dir
        method exists : bool
   end

class type file_elt = 
    object
        inherit [Utils.file] elt
        method file : Utils.file
        method exists : bool
    end

class type env_file_elt = 
    object
        inherit [Utils.file] env_elt
        method file : Utils.file
        method exists : bool
    end

(* Configuration *)

class config_t () = 
    object (self)
        val mutable cfg = IniBase.Ctx.empty
        method add_ini_file fname = 
            cfg <- IniLex.process (Lexing.from_string (Utils.get_file fname)) cfg
        method try_ini_file fname = 
            try self#add_ini_file fname
            with Parsing.Parse_error -> eprintf "Unable to parse file [%s].\n%!" fname
        method add_unix_env () = 
            cfg <- IniBase.merge_unix cfg

    end
class access_t () = 
    object (self)
        inherit config_t ()
        method get (var : evar_elt) = IniBase.get_env ~ctx:var#context var#env cfg
        method put (var : evar_elt) v = cfg <- IniBase.put_env ~ctx:var#context var#env v cfg
        method find (var : evar_elt) = IniBase.find_env ~ctx:var#context var#env cfg
        method rem (var : evar_elt) = cfg <- IniBase.rem_env ~ctx:var#context var#env cfg
        method add (var : evar_elt) s = cfg <- IniBase.put_env ~ctx:var#context var#env s cfg
        method update (bv : bvar_elt) (def : value) = 
            match self#get (bv :> evar_elt) with
            | None -> self#add (bv :> evar_elt) def
            | Some v -> self#add (bv :> evar_elt) v 
    end

class out_t () = 
    object
        inherit access_t ()
        method buffer b = IniBase.buffer b cfg
        method to_string () = IniBase.to_string cfg
        method output fout = IniBase.output fout cfg
        method print () = IniBase.print cfg
    end

class cfg_t () = 
    object (self)
        inherit out_t () as super
        val mutable specs = Specs.empty
        method register ((flag, arg, desc) as ss) = 
            if not !Sys.interactive && Specs.mem ss specs then 
                raise (Failure (sprintf "Flag [%s] already in use" flag))
            else specs <- Specs.add ss specs
        method param_list () = Specs.elements specs
    end
let cfg : cfg_elt = new cfg_t ()

let rec parse_params pp = 
    if !Sys.interactive then []
    else params_interactive pp
and params_interactive pp = 
    let extras = ref [] in
    let add v = extras := v :: !extras in
    Arg.parse (pp#param_list()) add "Invalid argument";
    List.rev !extras

let varSep = try Unix.getenv "VARSEP" with Not_found -> "_"

class ['a] env_t (ser : 'a OSer.elt) (var : 'a OVar.elt) = 
    object (self)
        method config = cfg
        method env = if var#context = "" then var#env else var#context^varSep^var#env
        method private putStr (s : string) = Unix.putenv self#env s
        method ser = ser
        method var = var
        method put v = self#putStr (ser#to_str v)
        method get () = 
            try self#ser#of_str (Unix.getenv self#env)
            with Not_found -> self#put self#var#default; self#get ()
       method desc = OVar.desc (self#var :> bvar_elt)
       method arg = (var#switch, Arg.String self#putStr, self#desc)
       initializer
            self#config#register self#arg;
            self#config#update (var :> bvar_elt) (self#ser#to_str (self#get()))
    end

class ['a] t (ser : 'a OSer.elt) (var : 'a OVar.elt) = 
    object (self)
        inherit ['a] env_t ser var as super
        method private putStr (s : string) = cfg#put (var :> evar_elt) s
        method config = cfg
        method put v = 
            super#put v;
            self#putStr (ser#to_str v)
        method get () = 
            match cfg#get (var :> evar_elt) with
            | None ->  super#get ()
            | Some s -> ser#of_str s
    end

class dir_t (v : Utils.dir OVar.elt) =
    object (self)
        inherit [Utils.dir] t OSer.ser_str (of_bvar (v :> bvar_elt) v#default)
        method dir = self#get ()
        method exists = Sys.file_exists self#dir
    end

class file_t (v : Utils.file OVar.elt) =
    object (self)
        inherit [Utils.file] t OSer.ser_str (of_bvar (v :> bvar_elt) v#default)
        method file = self#get ()
        method exists = Sys.file_exists self#file
    end

class env_file_t (v : Utils.file OVar.elt) = 
    object (self)
        inherit [Utils.file] t OSer.ser_str (of_bvar (v :> bvar_elt) v#default) as super
        method file = self#get ()
        method exists = Sys.file_exists self#file
    end

class flag_t (bv : bvar_elt) (def : bool) = 
    object (self)
        inherit [bool] t OSer.ser_bool (of_bvar bv def) as super
        method arg = (self#var#switch, Arg.Unit (fun () -> self#put (not def)), OVar.desc bv)
    end

class str_t v = 
    object
        inherit [string] t ser_str v
    end
class int_t v =
    object (self)
        inherit [int] t ser_int v
    end
class int0_t (v : bvar_elt) = 
    object
        inherit int_t (OVar.of_bvar v 0)
    end
class int32_t (ov : int32 OVar.elt) = 
    object
        inherit [int32] t ser_int32 ov
    end
class int64_t (ov : int64 OVar.elt) = 
    object
        inherit [int64] t ser_int64 ov
    end
class flt_t (ov : float OVar.elt) = 
    object
        inherit [float] t ser_flt ov
    end
class chr_t (ov : char OVar.elt) =
    object
        inherit [char] t ser_chr ov
    end
class bool_t (ov : bool OVar.elt) = 
    object
        inherit [bool] t ser_bool ov
    end
class bytes_t (ov : bytes OVar.elt) = 
    object
        inherit [bytes] t ser_bytes ov
    end

class flag_set_t (ov : bvar_elt) = 
    object
        inherit flag_t ov false
    end
class flag_clear_t (ov : bvar_elt) = 
    object
        inherit flag_t ov true
    end

class ['a] option_t (ser : 'a OSer.elt) (nul : string) (v :'a option OVar.elt) = 
    object
        inherit ['a option] t (OSer.make_opt ser nul) v
    end
class ['a] option_none_t (ser : 'a OSer.elt) (nul : string) (v : OVar.bvar_elt) = 
    object
        inherit ['a] option_t ser nul (OVar.none_of_bvar v) 
    end
class ['a] opt_none_t (ser : 'a OSer.elt) (bv : bvar_elt) =
    object
        inherit ['a] option_none_t ser "" bv
    end

class ['a] list_t (ser : 'a OSer.elt) (sep : OSer.sep_elt) (v : 'a list OVar.elt) =
    object
        inherit ['a list] t (OSer.make_list ser sep) v
    end 
class ['a] list_null_t (ser : 'a OSer.elt) (sep : OSer.sep_elt) (bv : bvar_elt) = 
    object
        inherit ['a] list_t ser sep (OVar.nil_of_bvar bv)
    end
class str_list_t (sep : OSer.sep_elt) (var : string list OVar.elt) = 
    object
        inherit ['a] list_t ser_str sep var
    end
class ['a] comma_list_t (ser : 'a OSer.elt) (var : 'a list OVar.elt) = 
    object
        inherit ['a] list_t ser OSer.comma var
    end
class ['a] space_list_t (ser : 'a OSer.elt) (var : 'a list OVar.elt) = 
    object
        inherit ['a] list_t ser OSer.space var
    end
class ['a] colon_list_t (ser : 'a OSer.elt) (var : 'a list OVar.elt) = 
    object
        inherit ['a] list_t ser OSer.colon var
    end
class ['a] semicolon_list_t (ser : 'a OSer.elt) (var : 'a list OVar.elt) = 
    object
        inherit ['a] list_t ser OSer.semicolon var
    end
class ['a] bar_list_t (ser : 'a OSer.elt) (var : 'a list OVar.elt) = 
    object
        inherit ['a] list_t ser OSer.bar var
    end

class inet_t (v : inet_addr OVar.elt) = 
    object
        inherit [inet_addr] t OSer.ser_inet v
    end
class inet_any_t (v : OVar.bvar_elt) = 
    object
        inherit inet_t (inet_any_of_bvar v)
    end

class ['a,'b] tuple_t (sep : OSer.sep_elt) (ser1 : 'a OSer.elt) (ser2 : 'b OSer.elt) (var : ('a * 'b) OVar.elt) = 
    object (self)
        inherit ['a * 'b] t (OSer.make_tuple sep ser1 ser2) var
        method fst = fst (self#get())
        method snd = snd (self#get())
    end
class ['a,'b] comma_tuple_t (ser1 : 'a OSer.elt) (ser2 : 'b OSer.elt) (var : ('a * 'b) OVar.elt) = 
    object
        inherit ['a,'b] tuple_t OSer.comma ser1 ser2 var
    end
class ['a,'b] space_tuple_t (ser1 : 'a OSer.elt) (ser2 : 'b OSer.elt) (var : ('a * 'b) OVar.elt) = 
    object
        inherit ['a,'b] tuple_t OSer.space ser1 ser2 var
    end
class ['a,'b] colon_tuple_t (ser1 : 'a OSer.elt) (ser2 : 'b OSer.elt) (var : ('a * 'b) OVar.elt) = 
    object
        inherit ['a,'b] tuple_t OSer.colon ser1 ser2 var
    end 
class ['a,'b] semicolon_tuple_t (ser1 : 'a OSer.elt) (ser2 : 'b OSer.elt) (var : ('a * 'b) OVar.elt) = 
    object
        inherit ['a,'b] tuple_t OSer.semicolon ser1 ser2 var
    end 
class ['a,'b] bar_tuple_t (ser1 : 'a OSer.elt) (ser2 : 'b OSer.elt) (var : ('a * 'b) OVar.elt) = 
    object
        inherit ['a,'b] tuple_t OSer.bar ser1 ser2 var
    end

let config_file (fv : Utils.file OVar.elt) : file_elt = 
    object (self)
        inherit file_t fv as super
        method private default = ".conf"
        method private find fn = 
            match Utils.find_file ~env:fv#env ~ext:".conf" fn with
            | None -> Utils.touch self#default; self#default
            | Some f -> f
        method put x = super#put (self#find x)
    end

let home_file (bv : bvar_elt) : env_file_elt = 
    let def = 
        let exe = Sys.argv.(0) in
        let no_ext = try Utils.chop_ext exe with e -> exe in
        "."^(Filename.basename no_ext)
    in
    object (self)
        inherit env_file_t (of_bvar bv def) as super
        method private file_from_name () = 
            let bname = Filename.basename Sys.argv.(0) in
            let sfx = try Utils.chop_ext bname with Invalid_argument _ -> bname in
            "." ^ sfx
        method private find () =
            let ff = super#file in
            let rec f_exists () = 
                if Sys.file_exists ff then Some ff
                else try_env (Filename.basename ff)
            and try_env bf = 
                let hf = Filename.concat (Unix.getenv "HOME") bf in
                if Sys.file_exists hf then Some hf else None
            in 
            match ff with
            | "" -> self#put (self#file_from_name()); self#find()
            | _ -> f_exists()
        method file =
            let try_get () = 
                match self#get() with
                | "" -> self#file_from_name ()
                | fn -> fn
            in
            match self#find () with
            | None -> try_get ()
            | Some fn -> fn
    end

class type base_elt = 
    object
        method config : cfg_elt
        method program : string
        method ident : string
        method args : string list
        method error : exn -> string
        method init : file_elt -> unit
    end

class base_t =
    object (self)
        val mutable args = Array.to_list Sys.argv
        method config = cfg
        method program = Filename.basename Sys.argv.(0)
        method ident = try Utils.chop_ext self#program with _ -> self#program
        method args = args
        method error e = Utils.error e
        method init  (cf : file_elt) = 
            let fname = cf#file in
            if cf#exists then self#config#try_ini_file cf#file else ();
            args <- parse_params self#config;
            if fname <> cf#file && cf#exists then
                begin
                    self#config#try_ini_file cf#file;
                    args <- parse_params self#config
                end
    end

let new_str v = new str_t v
let new_int v = new int_t v
let new_int0 v = new int0_t v
let new_int32 v = new int32_t v
let new_int64 v = new int64_t v
let new_flt v = new flt_t v
let new_chr v = new chr_t v
let new_bool v = new bool_t v
let new_bytes v = new bytes_t v
let new_flag_set v = new flag_set_t v
let new_flag_clear v = new flag_clear_t v

let make_str c e def sw de = new_str (OVar.make c e def sw de)
let make_int c e def sw de = new_int (OVar.make c e def sw de)
let make_int0 c e sw de = new_int0 (OVar.bmake c e sw de)
let make_int32 c e def sw de = new_int32 (OVar.make c e def sw de)
let make_int64 c e def sw de = new_int64 (OVar.make c e def sw de)
let make_flt c e def sw de = new_flt (OVar.make c e def sw de)
let make_chr c e def sw de = new_chr (OVar.make c e def sw de)
let make_bool c e def sw de = new_bool (OVar.make c e def sw de)
let make_bytes c e def sw de = new_bytes (OVar.make c e def sw de)
let flag_set c e sw de = new_flag_set (OVar.bmake c e sw de)
let flag_clear c e sw de = new_flag_clear (OVar.bmake c e sw de)
let make_home_file c e sw de = home_file (OVar.bmake c e sw de)


