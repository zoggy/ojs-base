
open Js

class type document =
  object

  end

class type ace =

  object
    method edit : js_string t -> unit meth
    method createEditSession : js_string t -> js_string t -> document meth
  end

let ace s = Unsafe.fun_call
  (Unsafe.variable "ace") [|Unsafe.inject (Js.string s)|]