open Unix
open Printf
open Pervasives

type addr = string

type return_code =
  [
  | `System_status                             (* 211 *)
  | `Help                                      (* 214 *)
  | `Domain_service_ready of string            (* 220 *)
  | `Service_closing of string                 (* 221 *)
  | `Okay                                      (* 250 *)
  | `User_forwarded of string                  (* 251 *)
  | `Verify_user_failed                        (* 252 *)
  | `Start_mail_input                          (* 354 *)
  | `Service_unavailable of string             (* 421 *)
  | `Mailbox_unavailable                       (* 550 *)
  | `User_not_local of string                  (* 551 *)
  | `Storage_exceeded                          (* 552 *)
  | `Incorrect_mailbox_name                    (* 553 *)
  | `Transaction_failed                        (* 554 *)
  | `Unknown of int * string                   (* *** *)
  ]

exception Error of return_code

module type PARAMS =
    sig
        val host : Utils.host
        val port : Utils.port
        val mail_from : addr
        val mail_to : addr list
        val subject : string
    end

module type ELT =
    sig
        val send : ?addr:Unix.sockaddr -> 
                   ?mail_from:addr -> 
                   ?subject:string -> addr -> string -> unit
        val group : ?addr:Unix.sockaddr -> 
                    ?mail_from:addr -> 
                    ?mail_to:addr list -> 
                    ?subject:string -> string -> unit
        val printf : ?addr:Unix.sockaddr -> 
                     ?mail_from:addr -> 
                     ?subject:string -> 
                     addr -> ('a, unit, string, unit) format4 -> 'a 
        val mprintf : ?addr:Unix.sockaddr -> 
                      ?mail_from:addr -> 
                      ?mail_to:addr list -> 
                      ?subject:string -> 
                      ('a, unit, string, unit) format4 -> 'a 
    end

let next_token lex = 
  let (code, msg) = SmtpLex.token lex in
  match int_of_string code with
  | 211     -> `System_status
  | 214     -> `Help
  | 220     -> `Domain_service_ready msg
  | 221     -> `Service_closing msg
  | 250     -> `Okay
  | 251     -> `User_forwarded msg
  | 252     -> `Verify_user_failed
  | 354     -> `Start_mail_input
  | 421     -> `Service_unavailable msg
  | 550     -> `Mailbox_unavailable
  | 551     -> `User_not_local msg
  | 552     -> `Storage_exceeded
  | 553     -> `Incorrect_mailbox_name
  | 554     -> `Transaction_failed
  | x       -> `Unknown (x, msg)


let string_of_return_code = function
  | `System_status                    -> "System_status"
  | `Help                             -> "Help"
  | `Domain_service_ready s           -> "Domain_service_ready("^s^")"
  | `Service_closing s                -> "Service_closing("^s^")"
  | `Okay                             -> "Okay"
  | `User_forwarded s                 -> "User_forwarded("^s^")"
  | `Verify_user_failed               -> "Verify_user_failed"
  | `Start_mail_input                 -> "Start_mail_input"
  | `Service_unavailable s            -> "Service_unavailable("^s^")"
  | `Mailbox_unavailable              -> "Mailbox_unavailable"
  | `User_not_local s                 -> "User_not_local("^s^")"
  | `Storage_exceeded                 -> "Storage_exceeeded"
  | `Incorrect_mailbox_name           -> "Incorrect_mailbox_name"
  | `Transaction_failed               -> "Transaction_failed"
  | `Unknown (rc, line)               -> 
      "Unknown("^(string_of_int rc)^","^line^")"
let g_Helo = "helo " 
let g_MailFrom = "mail from: "
let g_RcptTo = "rcpt to: "
let g_Data = "data"
let g_From = "From: "
let g_Subject = "Subject: "
let g_EndData = "."
let g_Quit = "quit"

let newline fout = 
  output_string fout "\r\n";
  flush fout
let writeln fout str = 
  output_string fout str;
  newline fout
let domain_service_ready lex =
  match next_token lex with
  | `Domain_service_ready _ -> ()
  | x -> raise (Error x)
let ok lex = 
  match next_token lex with
  | `Okay -> ()
  | x -> raise (Error x)
let data_ok lex = 
  match next_token lex with
  | `Start_mail_input -> ()
  | x -> raise (Error x)
let service_closing lex = 
  match next_token lex with
  | `Service_closing _ -> ()
  | x -> raise (Error x)

let domain mfrom =
  try
    let len = String.length mfrom in
    let idx = 1 + String.index mfrom '@' in
    String.sub mfrom idx (len - idx)
  with Not_found ->
    raise (Failure ("Invalid 'From':  name@domain"))

let helo mfrom lex fout =
  domain_service_ready lex;
  output_string fout g_Helo;
  writeln fout (domain mfrom);
  ok lex
let mailfrom mfrom lex fout =
  output_string fout g_MailFrom;
  writeln fout mfrom;
  ok lex
let rcptto mto lex fout =
  output_string fout g_RcptTo;
  writeln fout mto;
  ok lex
let data mfrom msubj mdata lex fout =
  writeln fout g_Data;
  data_ok lex;
  writeln fout (g_From^mfrom);
  output_string fout g_Subject;
  writeln fout msubj;
  newline fout;
  writeln fout mdata;
  writeln fout g_EndData;
  ok lex
let quit lex fout = 
  writeln fout g_Quit;
  service_closing lex

let send_msg ~addr ~mail_from ~mail_to ~subject msg =
  let send' lex fout = 
    helo mail_from lex fout;
    mailfrom mail_from lex fout;
    rcptto mail_to lex fout;
    data mail_from subject msg lex fout;
    quit lex fout
  in
  let fd = socket PF_INET SOCK_STREAM 0 in
  try
    connect fd addr;
    send' (Lexing.from_channel (in_channel_of_descr fd)) 
      (out_channel_of_descr fd);
    Unix.close fd
  with e ->
    (try Unix.close fd with _ -> ());
    raise e

module Make(P : PARAMS) = 
    struct
        include P
        let ia = Utils.get_inet P.host
        let sa = ADDR_INET(ia, P.port)
        let old_send_msg = send_msg

        let send ?(addr=sa) ?(mail_from=P.mail_from) ?(subject=P.subject) mail_to msg =
            send_msg ~addr ~mail_from ~mail_to ~subject msg

        let group ?(addr=sa) ?(mail_from=P.mail_from) ?(mail_to=P.mail_to) 
                  ?(subject=P.subject) msg =
            let send mto = send ~addr ~mail_from ~subject mto msg in
            List.iter send mail_to

        let printf ?(addr=sa) ?(mail_from=P.mail_from) ?(subject=P.subject) mail_to fmt =
            let send msg = send ~addr ~mail_from ~subject mail_to msg in
            ksprintf send fmt

        let mprintf ?(addr=sa) ?(mail_from=P.mail_from) ?(mail_to=P.mail_to) 
                    ?(subject=P.subject) fmt =
            let send msg = group ~addr ~mail_from ~mail_to ~subject msg in
            ksprintf send fmt

    end
