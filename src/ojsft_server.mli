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

(** Server side of filetree *)

module type S =
  sig
    module P : Ojsft_types.P
    val access_forbidden : Ojs_path.t -> P.server_msg
    val creation_forbidden : Ojs_path.t -> P.server_msg
    val deletion_forbidden : Ojs_path.t -> P.server_msg
    val renaming_forbidden : Ojs_path.t -> Ojs_path.t -> P.server_msg
    class filetree :
      (P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.server_msg -> unit Lwt.t) ->
        id:string ->
        Ojs_path.t ->
        object
          val mutable file_filter : Ojs_path.t -> bool
          method after_add_file : Ojs_path.t -> unit
          method after_get_tree :
            Ojsft_types.file_tree list -> Ojsft_types.file_tree list
          method before_add_file : Ojs_path.t -> unit
          method can_add_dir : string -> bool
          method can_add_file : string -> bool
          method can_delete : string -> bool
          method can_rename : string -> string -> bool
          method handle_add_dir :
            (P.server_msg -> unit Lwt.t) -> Ojsft_types.path -> unit Lwt.t
          method handle_add_file :
            (P.server_msg -> unit Lwt.t) ->
            Ojsft_types.path -> string -> unit Lwt.t
          method handle_call :
            (P.server_msg -> unit Lwt.t) -> P.client_msg -> unit Lwt.t
          method handle_delete :
            (P.server_msg -> unit Lwt.t) -> Ojsft_types.path -> unit Lwt.t
          method handle_message :
            (P.server_msg -> unit Lwt.t) -> P.client_msg -> unit Lwt.t
          method handle_rename :
            (P.server_msg -> unit Lwt.t) ->
            Ojsft_types.path -> Ojsft_types.path -> unit Lwt.t
          method id : string
          method root : Ojs_path.t
          method set_file_filter : (Ojs_path.t -> bool) -> unit
        end
      class filetrees :
        (P.app_server_msg -> (P.app_client_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_server_msg -> unit Lwt.t) ->
        ((P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.server_msg -> unit Lwt.t) -> id:string -> Ojs_path.t -> filetree) ->
        object
          val mutable filetrees : filetree Ojs_server.SMap.t
          method add_filetree :
            id:Ojs_server.SMap.key -> Ojs_path.t -> filetree
          method filetree : Ojs_server.SMap.key -> filetree
          method handle_call :
            (P.app_server_msg -> unit Lwt.t) ->
            P.app_client_msg -> unit Lwt.t
          method handle_message :
            (P.app_server_msg -> unit Lwt.t) ->
            P.app_client_msg -> unit Lwt.t
        end
    end
module Make :
  functor (P : Ojsft_types.P) -> S
  with type P.server_msg = P.server_msg
   and type P.client_msg = P.client_msg
   and type P.app_server_msg = P.app_server_msg
   and type P.app_client_msg = P.app_client_msg

