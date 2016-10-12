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

(** JS utilities *)

module SMap : Map.S with type key = string

(** Replace a referenced map by the same map with an additional binding. *)
val ( += ) : 'a SMap.t ref -> string * 'a -> unit

(** Replace a referenced map by the same map minus a binding. *)
val ( -= ) : 'a SMap.t ref -> string -> unit

type id = string

(** Log to JS console. *)
val log : string -> unit

(** Helper function to read a messsage from a websocket frame data,
  using the given (json to msg) function. *)
val mk_msg_of_wsdata :
  (Yojson.Safe.json -> ('a, string) result) ->
  string -> 'a option

(** Used to create ojs specific classes. [class_ str] returns ["ojs-"^str]. *)
val class_ : string -> string

(** [setup_ws url msg_of_data ~onopen ~onmessage] opens a websocket
  connection to the given url, using [msg_of_data] to read messages and
  pass them to the [onmessage] handler. [onopen] is called when the
  websocket is open. *)
val setup_ws :
  string ->
  (string -> 'a option) ->
  onopen:(WebSockets.webSocket Js.t -> 'c) ->
  onmessage:(WebSockets.webSocket Js.t -> 'a -> 'd) ->
  WebSockets.webSocket Js.t option

(** Send data on the given websocket. *)
val send_msg : WebSockets.webSocket Js.t -> string -> unit

(** Remove all children of a DOM node. *)
val clear_children : #Dom.node Js.t -> unit

(** Get a DOM node by its id.
 @raise Failure if the node could not be found.*)
val node_by_id : string -> Dom_html.element Js.t

(** [gen_id()] returns a new id prefixed by ["ojsid"]. This is useful
  to create unique node ids when creating nodes in the DOM. *)
val gen_id : unit -> id

(** [set_onclick node f] sets [f] as callback to "onclick" event on [node]. *)
val set_onclick :
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t -> 'a) -> unit

(*i==v=[String.split_string]=1.2====*)
(** Separate the given string according to the given list of characters.
@author Maxence Guesdon
@version 1.2
@param keep_empty is [false] by default. If set to [true],
   the empty strings between separators are kept.
@cgname String.split_string*)
val split_string : ?keep_empty:bool -> string -> char list -> string list
(*/i==v=[String.split_string]=1.2====*)

(** [get_classes node] returns the list of classes associated to this element
  (by splitting on spaces in the class attribute). *)
val get_classes : Dom_html.element Js.t -> string list

(** [node_unset_class node cl] removes the given class [cl] from the given
  DOM element. *)
val node_unset_class : Dom_html.element Js.t -> string -> unit

(** [node_set_class node cl] adds the given class [cl] from the given
  DOM element, if it is not present yet. *)
val node_set_class : Dom_html.element Js.t -> string -> unit

(** [unset_class ~id cl] is the same as {!node_unset_class} but use
  the given [id] to retrieve the element. *)
val unset_class : id:string -> string -> unit

(** [set_class ~id cl] is the same as {!node_set_class} but use
  the given [id] to retrieve the element. *)
val set_class : id:string -> string -> unit
