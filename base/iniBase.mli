open Unix
open Printf
open Pervasives

type var = string
type value = string
type pair = var * value
type context = string

type line = 
    | Context of context
    | Pair of pair
    | Eof

module Props : Map.S with type key = var
module Ctx : Map.S with type key = context

type props = value Props.t
type cfg = props Ctx.t

val default_context : context               (* INI_DEFAULT_CONTEXT *)
val context_sep : string                    (* INI_CONTEXT_SEP *)
val context_name : context -> var -> var

val get_props : context -> cfg -> props option
val put_props : context -> props -> cfg -> cfg
val find_props : context -> cfg -> props
val rem_props : context -> cfg -> cfg

val get_var : var -> props -> value option
val put_var : var -> value -> props -> props
val find_var : var -> props -> value
val rem_var : var -> props -> props

val get_cfg : context * var -> cfg -> value option
val put_cfg : context * var -> value -> cfg -> cfg
val find_cfg : context * var -> cfg -> value
val rem_cfg : context * var -> cfg -> cfg

val get_env : ?ctx:context -> var -> cfg -> value option
val put_env : ?ctx:context -> var -> value -> cfg -> cfg
val find_env : ?ctx:context -> var -> cfg -> value
val rem_env : ?ctx:context -> var -> cfg -> cfg

val unix_props : unit -> props
val add_unix : ?ctx:context -> cfg -> cfg
val merge_unix : ?ctx:context -> cfg -> cfg
val init_unix : ?ctx:context -> unit -> cfg

val merge : cfg -> props

val new_cfg : cfg -> 
      (?ctx:context -> var -> value option) * 
      (?ctx:context -> var -> value -> unit) * 
      (?ctx:context -> var -> value) 

val new_cfg_ctx : cfg -> context ->
    (var -> value option) * (var -> value -> unit) * (var -> value)

val new_unix : ?ctx:context -> cfg ->
    (?ctx:context -> var -> value option) *
    (?ctx:context -> var -> value -> unit) *
    (?ctx:context -> var -> value)

val buffer_props : Buffer.t -> props -> unit
val buffer_context : Buffer.t -> context -> props -> unit
val buffer : Buffer.t -> cfg -> unit

val string_of_props : props -> string
val string_of_context : context -> props -> string
val to_string : cfg -> string

val output_props : out_channel -> props -> unit
val output_context : out_channel -> context -> props -> unit
val output : out_channel -> cfg -> unit

val print_props : props -> unit
val print_context : context -> props -> unit
val print : cfg -> unit

