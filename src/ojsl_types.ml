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

(** Types for lists. *)

module Base = struct
    type 'a server_msg = .. [@@deriving yojson]
    type 'a server_msg +=
      | SOk
        | SError of string
        | SAdd of 'a
        | SDelete of 'a
        | SList of 'a list
        [@@deriving yojson]

    type 'a client_msg = .. [@@deriving yojson]
    type 'a client_msg +=
      | Get
      | Add of 'a
        | Delete of 'a
        [@@deriving yojson]
  end

module Make_base() = struct include Base end

module type P =
  sig
    include (module type of Base)

    type elt
    type 'a msg [@@deriving yojson]

    val pack_msg : string -> 'msg -> (string * 'msg) msg
    val unpack_msg : (string * 'msg) msg -> (string * 'msg) option
  end


