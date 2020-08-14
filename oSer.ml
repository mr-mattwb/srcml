open Unix
open Printf
open Pervasives

exception Conversion of string * string

class type ['a] elt = 
    object
        method of_str : string -> 'a
        method to_str : 'a -> string
    end

class type sep_elt = 
    object
        method rex : Pcre.regexp
        method sep : string
    end

class ['a] t (ostr : string -> 'a) (tstr : 'a -> string) =
    object
        method of_str = ostr
        method to_str = tstr
    end

class sep_t (r : Pcre.regexp) (s : string) = 
    object
        method rex = r
        method sep = s
    end

let list_of_str (ser : 'a elt) (sep : sep_elt) (s : string) : 'a list =
    let ls = Pcre.split ~rex:sep#rex s in
    List.map ser#of_str ls
let str_of_list (ser : 'a elt) (sep : sep_elt) (ls : 'a list) =
    let buf = Buffer.create 1024 in
    let add v = Buffer.add_string buf (ser#to_str v) in
    let next v = 
        Buffer.add_string buf sep#sep;
        Buffer.add_string buf (ser#to_str v)
    in
    match ls with
    | [] -> ""
    | hd :: [] -> ser#to_str hd
    | hd :: tl ->
        add hd;
        List.iter next tl;
        Buffer.contents buf
    
class ['a] list_t (ser : 'a elt) (sep : sep_elt) = 
    object
        method of_str s = list_of_str ser sep s
        method to_str ls = str_of_list ser sep ls
    end

class ['a] opt_t (ser : 'a elt) (sdef : string) = 
    object
        method of_str s = 
            if s = sdef then None
            else Some (ser#of_str s)
        method to_str = function
            | None -> sdef
            | Some o -> ser#to_str o
    end
class ['a] null_opt_t (ser : 'a elt) = 
    object
        inherit ['a] opt_t ser ""
    end

let tuple_of_str (sera : 'a elt) (sep : sep_elt) (serb : 'b elt) (str : string) = 
    match Pcre.split ~rex:sep#rex str with
    | [] -> (sera#of_str "", serb#of_str "")
    | a :: [] -> (sera#of_str a, serb#of_str "")
    | a :: b :: _ -> (sera#of_str a, serb#of_str b)

let str_of_tuple (sera : 'a elt) (sep : sep_elt) (serb : 'b elt) ((a, b) : 'a * 'b) = 
    (sera#to_str a)^sep#sep^(serb#to_str b)

class ['a, 'b] tuple_t  (ae : 'a elt) (be : 'b elt) (s : sep_elt) = 
    object
        method of_str str = tuple_of_str ae s be str
        method to_str (a, b) = str_of_tuple ae s be (a, b)
    end

let id x = x
let int_of_str =
    let pat = "^\\s*(\\d+)([^\\d].*)?$" in
    let ints s = Pcre.replace ~pat ~templ:"$1" s in
    fun v -> ints v

let str = new t id id
let integer = new t (fun v -> int_of_string (int_of_str v))  string_of_int
let integer32 = new t (fun v -> Int32.of_string (int_of_str v)) Int32.to_string
let integer64 = new t (fun v -> Int64.of_string (int_of_str v)) Int64.to_string
let flt = new t float_of_string string_of_float
let chr =
    let of_str s = 
        match s with 
        | "" -> '\000'
        | cs -> String.get cs 0
    in
    new t of_str (String.make 1)
let byte = new t Bytes.of_string Bytes.to_string
let bools =
    let of_str s =
        eprintf "bools#of_str [%s]\n%!" s;
        if s = "" then false
        else 
            match String.get s 0 with
            |'0'|'f'|'F'|'n'|'N' -> false
            | _ -> true
    in
    new t of_str string_of_bool

let wspc = "[\r\t ]*"
let rex_wspc m = Pcre.regexp (wspc^m^wspc)
let rex_wspc_sep m sep = new sep_t (rex_wspc m) sep

let wspc_sep str = rex_wspc_sep "[\t ]+" str 

let space_sep = wspc_sep " "
let tab_sep = wspc_sep "\t"
let comma_sep = rex_wspc_sep "," ","
let colon_sep = rex_wspc_sep ":" ":"
let semicolon_sep = rex_wspc_sep ";" ";"
let bar_sep = rex_wspc_sep "\\|" "|"

let space_list ser = new list_t ser space_sep
let tab_list ser = new list_t ser tab_sep
let comma_list ser = new list_t ser comma_sep
let colon_list ser = new list_t ser colon_sep
let semicolon_list ser = new list_t ser semicolon_sep
let bar_list ser = new list_t ser bar_sep

let space_tuple ser1 ser2 = new tuple_t ser1 ser2 space_sep
let tab_tuple ser1 ser2 = new tuple_t ser1 ser2 tab_sep
let comma_tuple ser1 ser2 = new tuple_t ser1 ser2 comma_sep
let colon_tuple ser1 ser2 = new tuple_t ser1 ser2 colon_sep
let semicolon_tuple ser1 ser2 = new tuple_t ser1 ser2 semicolon_sep
let bar_tuple ser1 ser2 = new tuple_t ser1 ser2 bar_sep

(*
let ser_inet : inet_addr elt = 
    object
       method of_str s = 
            if s = "" then inet_addr_any
            else 
                try 
                    (gethostbyname s).h_addr_list.(0)
                with e ->
                    e_printf "gethostbyname" e "inet#of_sstr";
                    inet_addr_of_string s
        method to_str ia = 
            try
                (gethostbyaddr ia).h_name
            with e ->
                e_printf "gethostbyadr" e "inet#to_str";
                string_of_inet_addr ia
    end
let ser_saddr : sockaddr elt = 
    object (self)
        method private inet_of_str str = 
            let rex = Pcre.regexp ":" in
            match Pcre.split ~rex str with
            | [] -> ADDR_INET(inet_addr_any, 0)
            | host :: [] -> ADDR_INET(ser_inet#of_str host, 0)
            | host :: port :: _ -> ADDR_INET(ser_inet#of_str host, ser_int#of_str port)
        method of_str s = 
            if s = "" then raise (Conversion ("sockaddr", s))
            else 
                let at = String.get s 0 in
                let rest = String.sub s 1 (String.length s - 1) in
                match at with
                | 'i' | 'I' -> self#inet_of_str rest
                | 'u' | 'U' -> ADDR_UNIX rest
                | _ -> raise (Conversion ("sockaddr", s))
        method to_str = function
            | ADDR_UNIX p -> "U"^p
            | ADDR_INET (ia, p) -> sprintf "I%s:%d" (ser_inet#to_str ia) p
    end

let make_tuple (sep : sep_elt) (ser1 : 'a elt) (ser2 : 'b elt) : ('a * 'b) elt = 
    object
        method of_str ss = 
            match Pcre.split ~rex:sep#rex ~max:2 ss with
            | [] -> (ser1#of_str "", ser2#of_str "")
            | s1 :: [] -> (ser1#of_str s1, ser2#of_str "")
            | s1 :: s2 :: _ -> (ser1#of_str s1, ser2#of_str s2)
        method to_str (s1, s2) = 
            (ser1#to_str s1)^sep#sep^(ser2#to_str s2)
    end
let comma_tuple a b = make_tuple comma a b
let colon_tuple a b = make_tuple colon a b
let space_tuple a b = make_tuple space a b
*)
