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


(** Remote calls *)

module J = Yojson.Safe
type json = J.json

type call_id

module type B =
  sig
    include Ojs_types.App_msg

    type app_server_msg +=
      | SCall of call_id * app_server_msg
      | SReturn of call_id * app_server_msg

    type app_client_msg +=
      | Call of call_id * app_client_msg
      | Return of call_id * app_client_msg

    val pack_server_call : call_id -> app_server_msg -> app_server_msg
    val pack_server_return : call_id -> app_server_msg -> app_server_msg
    val pack_client_call : call_id -> app_client_msg -> app_client_msg
    val pack_client_return : call_id -> app_client_msg -> app_client_msg
  end

module Base :
  functor (P : Ojs_types.App_msg) ->
    B with type app_server_msg = P.app_server_msg
       and type app_client_msg = P.app_client_msg

module type S =
  sig
    type app_server_msg
    type app_client_msg
    type t
    val rpc_handler : (app_server_msg -> unit Lwt.t) -> t
    val call :
       t -> app_server_msg -> (app_client_msg -> 'a Lwt.t) -> 'a Lwt.t
    val return : t -> call_id -> app_server_msg -> unit Lwt.t
    val on_return : t -> call_id -> app_client_msg -> unit
  end

module Make_server : functor (P : B) ->
  S with type app_server_msg = P.app_server_msg
     and type app_client_msg = P.app_client_msg

module Make_client : functor (P : B) ->
  S with type app_server_msg = P.app_client_msg
     and type app_client_msg = P.app_server_msg
