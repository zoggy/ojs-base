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

(**Editor, client side *)

type mime_type = string

type session = {
  sess_file : Ojs_path.t;
  sess_mime : mime_type;
  sess_ace : Ojs_ace.editSession Js.t;
  mutable sess_changed : bool;
}

module PMap = Ojs_path.Map

val mk_button : string -> Dom_html.element Js.t

val is_editable_from_mime : mime_type -> bool

module type S =
  sig
    module P : Ojsed_types.P
    class editor :
      (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.client_msg -> unit Lwt.t) ->
        bar_id:string ->
        msg_id:string ->
        string ->
        object
          val mutable current : session option
          val mutable sessions : session PMap.t
          method changed_files : PMap.key list
          method changed_sessions : session list
          method display_error : string -> unit
          method display_filename : session -> unit
          method display_message : string -> unit
          method edit_file : ?mime:mime_type -> PMap.key -> unit Lwt.t
          method get_session : PMap.key -> session option
          method handle_message : P.server_msg -> bool Js.t
          method id : string
          method is_editable_from_mime : mime_type -> bool
          method load_from_server : session -> unit Lwt.t
          method msg_id : string
          method new_session : ?mime:mime_type -> PMap.key -> session
          method on_changed : session -> unit
          method reload : unit Lwt.t
          method reload_file : session -> unit Lwt.t
          method save : unit Lwt.t
          method save_changed_files : unit Lwt.t
          method save_file : session -> unit Lwt.t
          method simple_call :
            ?on_ok:(unit -> unit) -> P.client_msg -> unit Lwt.t
        end
      class editors :
        (P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_client_msg -> unit Lwt.t) ->
        ((P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) ->
         bar_id:string -> msg_id:string -> string -> editor) ->
        object
          val mutable editors : editor Ojs_js.SMap.t
          method get_editor : Ojs_js.SMap.key -> editor
          method get_msg_id : Ojs_js.SMap.key -> string
          method handle_message : P.app_server_msg -> bool Js.t
          method setup_editor :
            bar_id:string -> msg_id:string -> Ojs_js.SMap.key -> editor
        end
    end

module Make :
  functor (P : Ojsed_types.P) -> S
  with type P.app_server_msg = P.app_server_msg
   and type P.app_client_msg = P.app_client_msg
   and type P.server_msg = P.server_msg
   and type P.client_msg = P.client_msg
