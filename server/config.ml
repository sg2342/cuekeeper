(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Mirage

let stack = generic_stackv4 default_network
let conf = generic_kv_ro "conf"

let main =
  let packages = [
	package ~sublibs:["mem"] "irmin";
	package ~sublibs:["mirage"] "tls";
	package "mirage-http";
	package "nocrypto"
	] in
  foreign
    ~packages
    ~deps:[abstract nocrypto]
    "Unikernel.Main" (stackv4 @-> kv_ro @-> pclock @-> job)

let () =
  register "cuekeeper" [
    main $ stack $ conf $ default_posix_clock
  ]
