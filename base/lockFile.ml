open Unix
open Printf
open Pervasives

type fd = Unix.file_descr

let perms = 
    try int_of_string (Unix.getenv "SRCML_BASE_LOCKPERMS")
    with Not_found -> 0o600

let close fd = Unix.close fd
let create lf = 
    let fd = Unix.openfile lf [O_CREAT; O_RDWR] perms in
    ignore (lseek fd 0 SEEK_SET);
    at_exit (fun () -> try Unix.close fd with _ -> ());
    fd
let use fn lf = Utils.use create close fn lf
let lock fd = Unix.lockf fd F_LOCK 0
let unlock fd = Unix.lockf fd F_ULOCK 0
let locked fd = 
    try Unix.lockf fd F_TEST 0; true
    with e -> false
let luse fd fn args = 
    lock fd;
    try
        let rc = fn args in
        unlock fd;
        rc
    with e ->
        (try unlock fd with _ -> ());
        raise e


