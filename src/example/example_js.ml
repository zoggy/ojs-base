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


let msg_of_wsdata = Ojs_js.mk_msg_of_wsdata Example_types.server_msg_of_yojson

let wsdata_of_msg msg =
  Yojson.Safe.to_string (Example_types.client_msg_to_yojson msg)

let ref_send = ref ((fun _ -> Lwt.return_unit) : Example_types.App_msg.app_client_msg -> unit Lwt.t)
let send msg = !ref_send msg

module Rpc_base = Ojs_rpc.Base(Example_types.App_msg)
module Rpc = Ojs_rpc.Make_client(Rpc_base)
let rpc_handler = Rpc.rpc_handler send

let call = Rpc.call rpc_handler

module PList = struct
  include Example_types.PList
    let gensym = let cpt = ref 0 in fun () -> incr cpt; string_of_int !cpt
    let insert id elt =
      let node = Ojs_js.node_by_id id in
      let new_id = id^"-"^(gensym()) in
      let doc = Dom_html.document in
      let div = doc##createElement (Js.string "div") in
      div##setAttribute (Js.string "id", Js.string new_id);
      let text = doc##createTextNode (Js.string (string_of_int elt)) in
      Dom.insertBefore node div Js.null;
      Dom.appendChild div text ;
      new_id

  end

module Mylist = Ojsl_js.Make(PList)
module FT = Ojsft_js.Make(Example_types.FT)
module ED = Ojsed_js.Make(Example_types.ED)

let trees = new FT.trees call send (new FT.tree);;
let editors = new ED.editors call send (new ED.editor);;
let lists = new Mylist.elists call send (new Mylist.elist)
(*  (fun msg cb -> call (msg :> Example_types.client_msg)
    (function `Mylist_msg _ as msg -> cb msg | _ -> Lwt.return_unit))
    (fun msg -> send (msg :> Example_types.client_msg))
    (new Mylist.elist);;*)

let on_deselect ti path =
  Ojs_js.log (Printf.sprintf "Node %S deselected" (Ojs_path.to_string path))

let on_select (editor : ED.editor) ti kind path =
  Ojs_js.log (Printf.sprintf "Node %S selected" (Ojs_path.to_string path));
  match kind with
    `Dir -> ()
  | `File mime -> ignore(editor#edit_file ~mime path)

let onopen ws =
  ref_send := (fun msg -> Ojs_js.send_msg ws (wsdata_of_msg msg); Lwt.return_unit);
  let tree = trees#setup_filetree ~msg_id: "ojs-msg" "ft" in
  let editor = editors#setup_editor ~msg_id: "ojs-msg" ~bar_id: "bar" "ed" in
  tree#set_on_select (on_select editor);
  tree#set_on_deselect on_deselect ;
  ignore(lists#setup_list ~msg_id: "ojs-msg" "elist")

let onmessage ws msg =
  match msg with
  | Example_types.FT.SFiletree _ -> trees#handle_message msg
  | Example_types.ED.SEditor _  -> editors#handle_message msg
  | Rpc_base.SReturn (call_id, msg) -> Rpc.on_return rpc_handler call_id msg; Js._false
  | _ -> failwith "Unhandled message"


let _ = Ojs_js.setup_ws "ws://0.0.0.0:8080"
  msg_of_wsdata ~onopen ~onmessage

