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

(** *)


type path = string [@@deriving yojson]

module App_msg = Ojs_types.Make_app_msg()

module PList =
  struct
    type app_server_msg = App_msg.app_server_msg = ..[@@deriving yojson]
    type app_client_msg = App_msg.app_client_msg = ..[@@deriving yojson]
    include (Ojsl_types.Make_base())

    type 'a server_msg += SUpdate of 'a list [@@deriving yojson]
    type 'a client_msg += Clear [@@deriving yojson]

    type elt = int [@@deriving yojson]
    type app_server_msg += SMylist of string * elt server_msg [@@deriving yojson]
    type app_client_msg += Mylist of string * elt client_msg [@@deriving yojson]

    let pack_server_msg id msg = SMylist (id, msg)
    let unpack_server_msg = function SMylist (id, msg) -> Some (id, msg) | _ -> None

    let pack_client_msg id msg = Mylist (id, msg)
    let unpack_client_msg = function Mylist (id, msg) -> Some (id, msg) | _ -> None
  end

module FT = Ojsft_types.Default_P(App_msg)
module ED = Ojsed_types.Default_P(App_msg)

let server_msg_to_yojson = App_msg.app_server_msg_to_yojson
let server_msg_of_yojson = App_msg.app_server_msg_of_yojson

let client_msg_to_yojson = App_msg.app_client_msg_to_yojson
let client_msg_of_yojson = App_msg.app_client_msg_of_yojson
