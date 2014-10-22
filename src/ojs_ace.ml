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
    method getValue : js_string t meth
  end

class type ace =
  object
    method edit : js_string t -> editor Js.t meth
    method createEditSession : js_string t -> js_string t -> document meth
  end

let ace = ((Unsafe.variable "ace") : ace Js.t)