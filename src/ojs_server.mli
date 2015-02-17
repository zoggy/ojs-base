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

(** Server-side utilities. *)

module J = Yojson.Safe
module SMap : Map.S with type key = string

val mk_msg_of_wsdata :
  (J.json -> [< `Error of string | `Ok of 'a ]) -> string -> 'a option
val mk_send_msg :
  ('a -> string) -> (Websocket.Frame.t option -> 'b) -> 'a -> 'b Lwt.t
val mk_msg_stream :
  (string -> 'a option) -> Websocket.Frame.t Lwt_stream.t -> 'a Lwt_stream.t

val handle_messages :
  (string -> 'a option) ->
  ('b -> string) ->
  (('b -> 'c Lwt.t) -> unit Lwt.t) ->
  Websocket.Frame.t Lwt_stream.t ->
  (Websocket.Frame.t option -> 'c) -> unit Lwt.t

module type P =
  sig
    type app_server_msg = .. [@@deriving yojson]
    type app_client_msg = .. [@@deriving yojson]

    val pack_server_call :
      Ojs_rpc.call_id -> app_server_msg -> app_server_msg
    val pack_server_return :
      Ojs_rpc.call_id -> app_server_msg -> app_server_msg
    val pack_client_call :
      Ojs_rpc.call_id -> app_client_msg -> app_client_msg
    val pack_client_return :
      Ojs_rpc.call_id -> app_client_msg -> app_client_msg

    val msg_of_wsdata : string -> app_client_msg option
    val wsdata_of_msg : app_server_msg -> string
  end

module type S = sig
  module Rpc : Ojs_rpc.S
  class connection_group :
    object
      val mutable connections :
        ((Rpc.app_server_msg -> unit Lwt.t) * Rpc.t) list
      val mutable handle_message :
        (Rpc.app_server_msg -> unit Lwt.t) ->
        Rpc.t -> Rpc.app_client_msg -> unit Lwt.t
      method add_connection :
        Websocket.Frame.t Lwt_stream.t ->
        (Websocket.Frame.t option -> unit) -> unit Lwt.t
      method broadcall :
        Rpc.app_server_msg ->
        (Rpc.app_client_msg -> unit Lwt.t) -> unit Lwt.t
      method broadcast : Rpc.app_server_msg -> unit Lwt.t
      method handle_message :
        (Rpc.app_server_msg -> unit Lwt.t) ->
        Rpc.t -> Rpc.app_client_msg -> unit Lwt.t
      method remove_connection :
        (Rpc.app_server_msg -> unit Lwt.t) -> unit
      method set_handle_message :
        ((Rpc.app_server_msg -> unit Lwt.t) ->
         Rpc.t -> Rpc.app_client_msg -> unit Lwt.t) ->
        unit
    end
end

module Make :
  functor (P : P) -> S
    with type Rpc.app_server_msg = P.app_server_msg
     and type Rpc.app_client_msg = P.app_client_msg
