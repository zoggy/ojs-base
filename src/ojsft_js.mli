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

(** Filetree, client side *)

type node_type = [ `Dir | `File ]

type tree_node = {
  tn_id : Ojs_js.id;
  mutable tn_basename : string;
  mutable tn_path : Ojs_path.t;
  tn_span_id : Ojs_js.id;
  tn_subs_id : Ojs_js.id option;
  tn_type : node_type;
  mutable tn_subs : tree_node list;
}
val tree_nodes : tree_node Ojs_js.SMap.t ref
val button_bar_class : string
val button_class : string
val collapsed_class : string
val button_bar : string -> Dom_html.element Js.t
val add_button :
  string -> ?cls:string -> string -> #Dom.node Js.t -> Dom_html.element Js.t
val add_button_add_dir : string -> #Dom.node Js.t -> Dom_html.element Js.t
val add_button_delete : string -> #Dom.node Js.t -> Dom_html.element Js.t
val drag_class : string
val preventDefault : 'a -> unit
val stopPropagation : 'a -> unit
val expand_buttons :
  ?start:[< `Collapsed | `Expand > `Collapsed ] ->
  string ->
  Dom_html.element Js.t ->
  string -> Dom_html.element Js.t * Dom_html.element Js.t

module type S =
  sig
    module P : Ojsft_types.P
    class tree :
      (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.client_msg -> unit Lwt.t) ->
        msg_id:string ->
        Ojs_js.SMap.key ->
        object ('a)
          val mutable filetree : tree_node list
          val mutable on_deselect : 'a -> Ojsft_types.path -> unit
          val mutable on_select :
            'a ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          val mutable selected : (Ojs_js.id * Ojsft_types.path) option
          val mutable show_files : bool
          method add_dir : Ojsft_types.path -> string -> unit Lwt.t
          method add_file :
            [ `Dir | `File ] -> Ojsft_types.path -> File.file Js.t -> unit
          method build_from_tree : Ojsft_types.file_tree list -> unit
          method compare_tn : tree_node -> tree_node -> int
          method delete : Ojsft_types.path -> unit Lwt.t
          method display_error : string -> unit
          method handle_add_dir : Ojsft_types.path -> unit
          method handle_add_file :
            Ojsft_types.path -> Ojsft_types.mime_type -> unit
          method handle_delete : Ojsft_types.path -> unit
          method handle_drag_drop :
            [ `Dir | `File ] ->
              Ojsft_types.path -> Dom_html.element Js.t -> unit
          method handle_message : P.server_msg -> bool Js.t
          method id : Ojs_js.SMap.key
          method insert_dir : Ojsft_types.path -> unit
          method insert_file :
            Ojsft_types.path -> Ojsft_types.mime_type -> unit
          method insert_tn :
            Ojs_js.id ->
              tree_node -> Dom.node Js.t -> tree_node list -> tree_node list
          method msg_id : string
          method on_deselect : 'a -> Ojsft_types.path -> unit
          method on_select :
            'a ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          method prompt_add_dir : Ojsft_types.path -> unit Lwt.t
          method prompt_delete : Ojsft_types.path -> unit Lwt.t
          method set_on_deselect : ('a -> Ojsft_types.path -> unit) -> unit
          method set_on_select :
            ('a ->
             [ `Dir | `File of Ojsft_types.mime_type ] ->
               Ojsft_types.path -> unit) ->
              unit
          method set_onclick :
            Dom_html.element Js.t ->
              Ojs_js.SMap.key ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          method set_selected :
            Ojs_js.SMap.key ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          method set_show_files : bool -> unit
          method set_unselected : Ojs_js.SMap.key -> Ojsft_types.path -> unit
          method simple_call : P.client_msg -> unit Lwt.t
          method tree_node_by_path : Ojsft_types.path -> tree_node
          method update_tree : unit Lwt.t
        end

    class trees :
      (P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_client_msg -> unit Lwt.t) ->
        ((P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) -> msg_id:string -> string -> tree) ->
        object
          val mutable trees : tree Ojs_js.SMap.t
          method get_msg_id : Ojs_js.SMap.key -> string
          method get_tree : Ojs_js.SMap.key -> tree
          method handle_message : P.app_server_msg -> bool Js.t
          method setup_filetree : msg_id:string -> Ojs_js.SMap.key -> tree
        end
  end

module Make : functor (P : Ojsft_types.P) -> S
  with type P.app_server_msg = P.app_server_msg
   and type P.app_client_msg = P.app_client_msg
   and type P.server_msg = P.server_msg
   and type P.client_msg = P.client_msg
