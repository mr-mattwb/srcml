open Unix
open Printf
open Pervasives

include Ini.Make(IUnix(struct let ctx = "" end))

