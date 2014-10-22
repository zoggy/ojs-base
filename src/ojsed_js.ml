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

open Ojs_js

type editor_info = {
    ed_id : id ;
    bar_id : id ;
    msg_id : id ;
    fname_id : id ;
    ws : WebSockets.webSocket Js.t ;
    editor : Ojs_ace.editor Js.t ;
    mutable current_file : Ojsed_types.path option ;
    mutable sessions : Ojs_ace.editSession Js.t SMap.t ;
  }

let editors = ref (SMap.empty : editor_info SMap.t)

let get_session ed ?contents filename =
  try  SMap.find filename ed.sessions
  with Not_found ->
      let sess = Ojs_ace.newEditSession
        (match contents with None -> "" | Some s -> s) ""
      in
      ed.sessions <- SMap.add filename sess ed.sessions;
      sess
let msg_of_wsdata json =
  try
    match Ojsed_types.server_msg_of_yojson (Yojson.Safe.from_string json) with
      `Error s -> failwith (s ^ "\n" ^ json)
    | `Ok msg -> Some msg
  with
    e ->
      log (Printexc.to_string e);
      None

let wsdata_of_msg msg =
  Yojson.Safe.to_string (Ojsed_types.client_msg_to_yojson msg)

let send_msg ws id msg =
  let msg = `Editor_msg (id, msg) in
  Ojs_js.send_msg ws (wsdata_of_msg msg)

let get_editor id =
  try SMap.find id !editors
  with Not_found -> failwith (Printf.sprintf "Invalid editor id %S" id)

let display_filename ed fname =
  let node = Ojs_js.node_by_id ed.fname_id in
  Ojs_js.clear_children node ;
  let t = Dom_html.document##createTextNode (Js.string fname) in
  Dom.appendChild node t

let save ws ed_id =
  let ed = get_editor ed_id in
  match ed.current_file with
    None -> ()
  | Some file ->
      let contents = Js.to_string (ed.editor##getValue()) in
      send_msg ws ed_id (`Save_file (file, contents))

let edit_file ws id ?contents fname =
  let ed = get_editor id in
  let sess = get_session ed ?contents fname in
  ed.editor##setSession(sess);
  ed.current_file <- Some fname ;
  display_filename ed fname

let get_msg_id id = (get_editor id).msg_id

let handle_message ws msg =
   try
    (match msg with
     | `Editor_msg (id, t) ->
         match t with
           `File_contents (fname, contents) ->
             edit_file ws id ~contents fname
         | `Ok msg -> Ojsmsg_js.display_text_message (get_msg_id id) msg
         | `Error msg -> Ojsmsg_js.display_text_error (get_msg_id id) msg
         | _ -> failwith "Unhandled message received from server"
    );
    Js._false
  with
    e ->
      log (Printexc.to_string e);
      Js._false

let build_editor ws ~msg_id ~bar_id ~editor_id =
  let ed_id = editor_id in
  let editor = Ojs_ace.ace##edit (Js.string ed_id) in
  let bar = Ojs_js.node_by_id bar_id in
  let doc = Dom_html.document in
  let button = doc##createElement(Js.string "button") in
  let text = doc##createTextNode(Js.string "Save") in

  let fname_id = ed_id ^ "__filename" in
  let fname = doc##createElement(Js.string "span") in
  fname##setAttribute (Js.string "id", Js.string fname_id);
  fname##setAttribute (Js.string "class", Js.string "filename");

  Dom.appendChild bar button ;
  Dom.appendChild button text ;
  Dom.appendChild bar fname ;

  Ojs_js.set_onclick button (fun _ -> save ws ed_id);

  let ed = {
      ed_id ; bar_id ; msg_id ; fname_id ;
      ws ;
      editor ;
      current_file = None ;
      sessions = SMap.empty ;
    }
  in
  ed

let setup_editor ws ~msg_id ~bar_id ~editor_id =
  let editor = build_editor ws ~msg_id ~bar_id ~editor_id in
  editors += (editor_id, editor)

