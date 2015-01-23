(*********************************************************************************)
(*                Ojs-base                                                       *)
(*                                                                               *)
(*    Copyright (C) 2014 INRIA. All rights reserved.                             *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

open Js

class type undoManager =
  object

  end

let newUndoManager () =
  Unsafe.new_obj (Unsafe.variable "ace.UndoManager") [| |]

class type document =
  object

  end

class type editSession =
  object
    method getValue : js_string t meth
    method setValue : js_string t -> unit meth
    method getDocument : document t meth
    method setMode : js_string t -> unit meth
    method setUndoManager : undoManager t -> unit meth
  end

let newEditSession s mode =
  (Unsafe.new_obj (Unsafe.variable "ace.EditSession")
    [| Unsafe.inject (Js.string s) ; Unsafe.inject (Js.string mode) |] : editSession Js.t)

let createEditSession s mode =
  (Unsafe.new_obj (Unsafe.variable "ace.EditSession")
    [| Unsafe.inject (Js.string s) ; Unsafe.inject (Js.string mode) |] : editSession Js.t)

class type editor =
  object
    method setSession : editSession t -> unit meth
    method getSession : editSession t prop
    method getValue : js_string t meth
    method setFontSize : js_string t -> unit meth
    method setKeyboardHandler : js_string t -> unit meth
  end

class type mode =
  object
    method mode : js_string t prop
  end
class type modeList =
  object
    method getModeForPath : js_string t -> mode t meth
  end


class type ace =
  object
    method edit : js_string t -> editor Js.t meth
    method createEditSession : js_string t -> js_string t -> document meth
  end

let ace = ((Unsafe.variable "ace") : ace Js.t)
let modeList : modeList Js.t =
  Js.Unsafe.meth_call ace "require"
    [| Unsafe.inject (Js.string "ace/ext/modelist") |]

