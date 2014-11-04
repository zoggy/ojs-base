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

(** Displaying messages in web pages. *)

let base_class = Ojs_js.class_"msg"
let class_ s = base_class ^ "-" ^ s

let display_message ?(timeout=3000.0) ?(cl=class_"info") id msg_nodes =
  let doc = Dom_html.document in
  let node = Ojs_js.node_by_id id in
  let div = doc##createElement (Js.string "div") in
  Ojs_js.node_set_class div cl ;
  Ojs_js.node_set_class div base_class ;

  if timeout > 0. then
    ignore(Dom_html.window##setTimeout
     (Js.wrap_callback (fun () -> Dom.removeChild node div), timeout)
    )
  else
    (
     let b = doc##createElement (Js.string "span") in
     Ojs_js.node_set_class b (class_"close") ;
     let t = doc##createTextNode (Js.string "✘") in
     Ojs_js.set_onclick b (fun _ -> Dom.removeChild node div);
     Dom.appendChild div b ;
     Dom.appendChild b t
    );

  Dom.appendChild node div ;
  List.iter (Dom.appendChild div) msg_nodes

let display_error id nodes = display_message ~timeout: 0. ~cl: (class_"error") id nodes

let display_text_message ?timeout ?cl id text =
  let t = Dom_html.document##createTextNode (Js.string text) in
  display_message ?timeout ?cl id [t]

let display_text_error id text =
  let t = Dom_html.document##createTextNode (Js.string text) in
  display_error id [t]

