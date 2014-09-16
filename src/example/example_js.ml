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

let on_deselect ti name =
  Ojs_js.log (Printf.sprintf "Node %S deselected" name)

let on_select ti name =
  Ojs_js.log (Printf.sprintf "Node %S selected" name);
  Ojsed_js.send_msg ti.Ojsft_js.ws "ed" (`Get_file_contents name)

let onopen ws =
  Ojsft_js.setup_filetree ws ~on_select ~on_deselect "ft" ;
  Ojsed_js.setup_editor ws ~bar: "bar" ~editor: "ed"

let onmessage ws msg =
  match msg with
    `Filetree_msg msg -> Ojsft_js.handle_message ws (`Filetree_msg msg)
  | `Editor_msg msg -> Ojsed_js.handle_message ws (`Editor_msg msg)
  | _ -> failwith "Unhandled message"

let msg_of_wsdata json =
  try
    match Example_types.server_msg_of_yojson (Yojson.Safe.from_string json) with
      `Error s -> failwith (s ^ "\n" ^ json)
    | `Ok msg -> Some msg
  with
    e ->
      Ojs_js.log (Printexc.to_string e);
      None

let wsdata_of_msg msg =
  Yojson.to_string (Example_types.client_msg_to_yojson msg)

let _ = Ojs_js.setup_ws "ws://localhost:8080"
  msg_of_wsdata wsdata_of_msg
  ~onopen ~onmessage

