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

let (rpc_handler : Example_types.server_msg Ojs_rpc.t) = Ojs_rpc.rpc_handler ()

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
  Yojson.Safe.to_string (Example_types.client_msg_to_yojson msg)

let ref_send = ref ((fun _ -> ()) : Example_types.client_msg -> unit)
let send msg = !ref_send msg
let call msg callback =
  Ojs_rpc.call (fun msg -> send msg; Lwt.return_unit) rpc_handler msg callback

let trees = new Ojsft_js.trees call send;;
let editors = new Ojsed_js.editors call send;;

let on_deselect ti path =
  Ojs_js.log (Printf.sprintf "Node %S deselected" (Ojs_path.to_string path))

let on_select ti path =
  Ojs_js.log (Printf.sprintf "Node %S selected" (Ojs_path.to_string path));
  ignore(call (`Editor_msg ("ed", `Get_file_contents path))
   (function
    | `Editor_msg (_, msg) ->
        ignore(editors#handle_message (`Editor_msg ("ed", msg)));
        Lwt.return_unit
    | _ -> Lwt.return_unit))


let onopen ws =
  ref_send := (fun msg -> Ojs_js.send_msg ws (wsdata_of_msg msg));
  trees#setup_filetree ~on_select ~on_deselect ~msg_id: "ojs-msg" "ft" ;
  editors#setup_editor ~msg_id: "ojs-msg" ~bar_id: "bar" "ed"

let onmessage ws msg =
  match msg with
    `Filetree_msg _ as msg -> trees#handle_message msg
  | `Editor_msg _  as msg -> editors#handle_message msg
  | `Return (call_id, msg) -> Ojs_rpc.on_return rpc_handler call_id msg; Js._false
  | _ -> failwith "Unhandled message"


let _ = Ojs_js.setup_ws "ws://localhost:8080"
  msg_of_wsdata wsdata_of_msg
  ~onopen ~onmessage

