
open Js

class type document =
  object

  end

class type editSession =
  object
    method getValue : js_string t meth
    method setValue : js_string t -> unit meth
    method getDocument : document t meth
  end

let newEditSession s mode =
  (Unsafe.new_obj (Unsafe.variable "ace.EditSession")
    [| Unsafe.inject (Js.string s) ; Unsafe.inject (Js.string mode) |] : editSession Js.t)

class type editor =
  object
    method setSession : editSession t -> unit meth
  end

class type ace =
  object
    method edit : js_string t -> editor Js.t meth
    method createEditSession : js_string t -> js_string t -> document meth
  end

let ace = ((Unsafe.variable "ace") : ace Js.t)