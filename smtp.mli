
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

val next_token : Lexing.lexbuf -> return_code

val string_of_return_code : return_code -> string

val send_msg : addr:Unix.sockaddr -> mail_from:string -> mail_to:string -> 
    subject:string -> string -> unit

module Make(P : PARAMS) : ELT

