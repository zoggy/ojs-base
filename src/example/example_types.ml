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

type server_msg0 = [
  | Ojsft_types.server_msg Ojsft_types.msg
  | Ojsed_types.server_msg Ojsed_types.msg
  ]
  [@@deriving yojson]


type client_msg0 = [
  | Ojsft_types.client_msg Ojsft_types.msg
  | Ojsed_types.client_msg Ojsed_types.msg
  ]
  [@@deriving yojson]

(*type server_rpc_msg = server_msg0 Ojs_rpc.msg
  [@@deriving yojson]*)
type server_msg = [ server_msg0 |  server_msg Ojs_rpc.msg]
  [@@deriving yojson]

(*type client_rpc_msg = client_msg0 Ojs_rpc.msg
  [@@deriving yojson]*)
type client_msg = [ client_msg0 | client_msg Ojs_rpc.msg ]
  [@@deriving yojson]
