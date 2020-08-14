open Unix
open Printf
open Pervasives

type var = string
type value = string
type pair = var * value
type context = string

exception Ctx_not_found of context
exception Var_not_found of var

type line = 
    | Context of context
    | Pair of pair
    | Eof

module Props = Map.Make(String)
module Ctx = Map.Make(String)

type props = var Props.t
type cfg = props Ctx.t

let default_context = 
    try Unix.getenv "INI_DEFAULT_CONTEXT"
    with Not_found -> ""
let context_sep = 
    try Unix.getenv "INI_CONTEXT_SEP"
    with Not_found -> "__"

let rec context_name ctx n = 
    match ctx with
    | "" when default_context <> "" -> context_name default_context n
    | "" -> n
    | _  -> ctx ^ context_sep ^ n

let get_props ctx cfg = 
    try Some (Ctx.find ctx cfg)
    with Not_found -> None
let put_props ctx env cfg = 
    Ctx.add ctx env cfg
let find_props ctx cfg = 
    try Ctx.find ctx cfg
    with Not_found -> raise (Ctx_not_found ctx)
let rem_props ctx cfg = Ctx.remove ctx cfg
let merge_props ctx np cfg =
    if Ctx.mem ctx cfg then
        let merge a b c =  
            match b, c with
            | None, None -> None
            | None, Some v -> Some v
            | Some v, None -> Some v
            | Some x, Some _ -> Some x
        in
        let props = Ctx.find ctx cfg in
        Ctx.add ctx (Props.merge merge np props) cfg
    else 
        Ctx.add ctx np cfg

let get_var var props = 
    try Some (Props.find var props)
    with Not_found -> None
let put_var var v props = Props.add var v props
let find_var var props = 
    try Props.find var props
    with Not_found -> raise (Var_not_found var)
let rem_var var props = Props.remove var props

let get_cfg (ctx, var) cfg = 
    match get_props ctx cfg with
    | None -> None
    | Some props -> get_var var props
let put_cfg (ctx, var) v cfg =
    match get_props ctx cfg with
    | None -> put_props ctx (put_var var v Props.empty) cfg
    | Some props -> put_props ctx (put_var var v props) cfg
let find_cfg (ctx, var) cfg = 
    find_var var (find_props ctx cfg)
let rem_cfg (ctx, var) cfg = 
    match get_props ctx cfg with
    | None -> cfg
    | Some pr -> put_props ctx (rem_var var pr) cfg

let get_env ?(ctx="") ev cfg = get_cfg (ctx, ev) cfg
let put_env ?(ctx="") ev v cfg = put_cfg (ctx, ev) v cfg
let find_env ?(ctx="") ev cfg = find_cfg (ctx, ev) cfg
let rem_env ?(ctx="") ev cfg = rem_cfg (ctx, ev) cfg

let spliteq str = 
    match Pcre.split ~pat:"=" ~max:2 str with
    | [] -> ("", "")
    | x :: [] -> (x, "")
    | x :: y :: _ -> (x, y)
let unix_props () = 
    let accum ab map = 
        let (a, b) = spliteq ab in
        put_var a b map
    in
    Array.fold_right accum (environment()) Props.empty
let add_unix ?(ctx="") cfg = put_props ctx (unix_props()) cfg
let merge_unix ?(ctx="") cfg = merge_props ctx (unix_props()) cfg
let init_unix ?(ctx="") () = add_unix ~ctx Ctx.empty

let merge (cfg : props Ctx.t) = 
    let accum ctx props newp =
        let pfx = 
            match ctx with
            | "" -> ""
            | n -> n^"."
        in
        let accum' v a newp' = Props.add (pfx^v) a newp' in
        Props.fold accum' props newp
    in
    Ctx.fold accum cfg Props.empty

let new_cfg icfg =
    let cfg = ref icfg in
    let get ?(ctx="") ev = get_env ~ctx ev !cfg in
    let put ?(ctx="") ev v = cfg := put_env ~ctx ev v !cfg in
    let find ?(ctx="") ev = find_env ~ctx ev !cfg in
    get, put, find

let new_cfg_ctx icfg ctx = 
    let get, put, find = new_cfg icfg in
    get ~ctx, put ~ctx, find ~ctx

let new_unix ?(ctx="") icfg = new_cfg (add_unix ~ctx icfg)

let has_spaces =
    let rex = Pcre.regexp "[ \r\t\n]+" in
    fun s -> Pcre.pmatch ~rex s

let buffer_pair b var v = 
    if has_spaces v then bprintf b "%s = \"%s\"\n" var v
    else bprintf b "%s = %s\n" var v
let buffer_props b props = Props.iter (buffer_pair b) props
let buffer_ctx_name b = function
    | "" -> bprintf b "\n\n"
    | ctx -> bprintf b "\n[%s]\n" ctx
let buffer_context b ctx pls = 
    buffer_ctx_name b ctx; 
    buffer_props b pls
let buffer b cfg = Ctx.iter (buffer_context b) cfg

let string_of_props props = 
    let b = Buffer.create 1024 in
    buffer_props b props;
    Buffer.contents b
let string_of_context ctx p = 
    let b = Buffer.create 1024 in
    buffer_context b ctx p;
    Buffer.contents b
let to_string cfg = 
    let b = Buffer.create 1024 in
    buffer b cfg;
    Buffer.contents b

let output_props fout p = output_string fout (string_of_props p)
let output_context fout c p = output_string fout (string_of_context c p)
let output fout cfg = output_string fout (to_string cfg)
let print_props p = output_props stdout p
let print_context c p = output_context stdout c p
let print cfg = output stdout cfg

