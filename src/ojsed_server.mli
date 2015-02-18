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

(** Server side of editor *)

module type S = sig
    module P : Ojsed_types.P
    val access_forbidden : Ojs_path.t -> P.server_msg
    class editor :
      (P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.server_msg -> unit Lwt.t) ->
        id:string ->
        Ojs_path.t ->
        object
          method can_read_file : string -> bool
          method can_write_file : string -> bool
          method handle_call :
            (P.server_msg -> unit Lwt.t) -> P.client_msg -> unit Lwt.t
          method handle_get_file_contents :
            (P.server_msg -> unit Lwt.t) -> Ojsed_types.path -> unit Lwt.t
          method handle_message :
            (P.server_msg -> unit Lwt.t) -> P.client_msg -> unit Lwt.t
          method handle_save_file :
            (P.server_msg -> unit Lwt.t) ->
            Ojsed_types.path -> string -> unit Lwt.t
          method id : string
          method root : Ojs_path.t
        end
      class editors :
        (P.app_server_msg -> (P.app_client_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_server_msg -> unit Lwt.t) ->
        ((P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.server_msg -> unit Lwt.t) -> id:string -> Ojs_path.t -> editor) ->
        object
          val mutable editors : editor Ojs_server.SMap.t
          method add_editor : id:Ojs_server.SMap.key -> Ojs_path.t -> editor
          method editor : Ojs_server.SMap.key -> editor
          method handle_call :
            (P.app_server_msg -> unit Lwt.t) ->
            P.app_client_msg -> unit Lwt.t
          method handle_message :
            (P.app_server_msg -> unit Lwt.t) ->
            P.app_client_msg -> unit Lwt.t
        end
  end

module Make : functor (P : Ojsed_types.P) -> S
  with type P.server_msg = P.server_msg
   and type P.client_msg = P.client_msg
   and type P.app_server_msg = P.app_server_msg
   and type P.app_client_msg = P.app_client_msg
