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

let ref_send = ref ((fun _ -> ()) : Example_types.client_msg -> unit)
let send msg = !ref_send msg

let (rpc_handler : (Example_types.server_msg, Example_types.client_msg) Ojs_rpc.t) =
  Ojs_rpc.rpc_handler (fun msg -> send msg; Lwt.return_unit)

let call = Ojs_rpc.call rpc_handler

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

let trees = new Ojsft_js.trees call send (new Ojsft_js.tree);;
let editors = new Ojsed_js.editors call send (new Ojsed_js.editor);;
let lists = new Mylist.elists
  (fun msg cb -> call (msg :> Example_types.client_msg)
    (function `Mylist_msg _ as msg -> cb msg | _ -> Lwt.return_unit))
    (fun msg -> send (msg :> Example_types.client_msg))
    (new Mylist.elist);;

let on_deselect ti path =
  Ojs_js.log (Printf.sprintf "Node %S deselected" (Ojs_path.to_string path))

let on_select ti path =
  Ojs_js.log (Printf.sprintf "Node %S selected" (Ojs_path.to_string path));
  ignore(call (`Editor_msg ("ed", `Get_file_contents path))
   (function
    | `Editor_msg (id, msg) ->
        ignore(editors#handle_message (`Editor_msg (id, msg)));
        Lwt.return_unit
    | _ -> Lwt.return_unit))


let onopen ws =
  ref_send := (fun msg -> Ojs_js.send_msg ws (wsdata_of_msg msg));
  let tree = trees#setup_filetree ~msg_id: "ojs-msg" "ft" in
  tree#set_on_select on_select;
  tree#set_on_deselect on_deselect;
  editors#setup_editor ~msg_id: "ojs-msg" ~bar_id: "bar" "ed";
  lists#setup_list ~msg_id: "ojs-msg" "elist"

let onmessage ws msg =
  match msg with
    `Filetree_msg _ as msg -> trees#handle_message msg
  | `Editor_msg _  as msg -> editors#handle_message msg
  | `Return (call_id, msg) -> Ojs_rpc.on_return rpc_handler call_id msg; Js._false
  | _ -> failwith "Unhandled message"


let _ = Ojs_js.setup_ws "ws://localhost:8080"
  msg_of_wsdata wsdata_of_msg
  ~onopen ~onmessage

