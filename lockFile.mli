open Unix
open Printf
open Pervasives

type fd

val perms : int

val close : fd -> unit
val create : Utils.file -> fd
val use : (fd -> 'a) -> Utils.file -> 'a
val lock : fd -> unit
val unlock : fd -> unit
val locked : fd -> bool
val luse : fd -> ('a -> 'b) -> 'a -> 'b

