open Unix
open Printf
open Pervasives

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
    val dprintf : severity -> ('a, unit, string, bool) format4 -> 'a
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
module SeverityVar(E : SEVERITY_VAR) = Cfg.Int(E)

let msg_format sev msg =
  let tm = Unix.localtime (Unix.time()) in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d % 8d % 4d %s"
    (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec
    (Unix.getpid()) sev msg
let msg_write fout sev msg = 
    output_string fout (msg_format sev msg);
    output_char fout '\n';
    flush fout

module Severity = SeverityVar(
  struct
    let env = "LOG_SEVERITY"
    let default = 0
    let switch = "--log-severity"
    let desc = "Log severity.  Zero or less means less logging"
  end)

module Make(W : WRITER) = 
  struct
    let write sev msg = 
        if sev <= (Severity.get()) then (W.write sev msg; true)
        else true
    let dprintf sev fmt = kprintf (write sev) fmt
  end

module type CHANNEL =
    sig
        val channel : out_channel
    end
module Channel_Writer(C : CHANNEL) = 
    struct
        let write sev msg = 
            try msg_write C.channel sev msg
            with Sys_error _ -> ()
    end
module Channel(C : CHANNEL) = Make(Channel_Writer(C))

module Stderr_Writer = Channel_Writer(
    struct
        let channel = stderr
    end)
module Stderr = Make(Stderr_Writer)

module Stdout_Writer = Channel_Writer(
    struct
        let channel = stdout
    end)
module Stdout = Make(Stdout_Writer)

module type FILE = 
    sig
        val file : Utils.file
    end
module File_Writer(F : FILE) = 
    struct
        let write sev msg = 
            Utils.file_out_app
                (fun fout -> msg_write fout sev msg)
                F.file
    end
module File(F : FILE) = Make(File_Writer(F))

module NoLog_Writer : WRITER = 
    struct
        let write sev msg = ()
    end
module NoLog : ELT = 
    struct
        let writer msg = true
        let dprintf sev fmt = kprintf writer fmt
    end

