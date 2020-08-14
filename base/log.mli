
type severity = int
(* 0 - Emergency
 * 1 - Alert
 * 2 - Crit
 * 3 - Err
 * 4 - Warning
 * 5 - Notice
 * 6 - Info
 * n - Debug
 *)

type 'a t = severity -> ('a, unit, string, bool) format4 -> 'a

module type WRITER =
  sig
    val write : severity -> string -> unit
  end
module type ELT =
  sig
    val dprintf : 'a t
  end
type log_elt = (module ELT)

module type SEVERITY_VAR =
    sig
        open Cfg
        val env : env
        val default : severity
        val switch : switch
        val desc : desc
    end
module type SEVERITY = Cfg.ELT with type elt = severity
module SeverityVar(S : SEVERITY_VAR) : SEVERITY

val msg_format : severity -> string -> string
val msg_write : out_channel -> severity -> string -> unit

(* [LOG_SEVERITY][0][--log-severity]Zero or less is only emergencies] *)
module Severity : SEVERITY

module Make(W : WRITER) : ELT

module type CHANNEL =
    sig
        val channel : out_channel
    end
module Channel_Writer(C : CHANNEL) : WRITER
module Channel(C : CHANNEL) : ELT

module Stderr_Writer : WRITER
module Stderr : ELT 

module Stdout_Writer : WRITER
module Stdout : ELT 

module type FILE = 
    sig
        val file : Utils.file
    end
module File_Writer(F : FILE) : WRITER
module File(F : FILE) : ELT

module NoLog_Writer : WRITER
module NoLog : ELT

