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

(** *)

module J = Yojson.Safe

let (>>=) = Lwt.bind
let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever

let wsdata_of_msg msg = J.to_string (Example_types.server_msg_to_yojson msg)
let msg_of_wsdata = Ojs_server.mk_msg_of_wsdata Example_types.client_msg_of_yojson

let rights path =
  let filename = Ojs_path.to_string path in
  match String.lowercase (Ojs_misc.filename_extension filename) with
    "ml" | "mli" -> Some `RO
  | "txt" | "html"-> Some `RW
  | _ -> None

let filepred =
  let re = Str.regexp "^\\(\\(cm.*\\)\\|[oa]\\|\\(\\annot\\)\\)$" in
  fun path ->
    let base = Ojs_path.basename path in
    String.length base > 0 &&
      String.get base 0 <> '.' &&
      (
       match String.lowercase (Ojs_misc.filename_extension base) with
         s when Str.string_match re s 0 -> false
       | _ -> true
      )


let connections = new Ojs_server.connection_group msg_of_wsdata wsdata_of_msg
let filetrees = new Ojsft_server.filetrees connections#broadcall connections#broadcast
let root =
  let root = try Sys.argv.(1) with _ -> "." in
   if Ojs_path.is_absolute root
  then root
  else Ojs_path.normalize (Ojs_path.append_path (Ojs_path.of_string (Sys.getcwd())) root)

let () = filetrees#add_filetree "ft" root

let handle_message send_msg rpc msg =
  match msg with
    `Filetree_msg t -> filetrees#handle_message send_msg (`Filetree_msg t)
  | `Editor_msg t -> Ojsed_server.handle_message ~rights root push_msg (`Editor_msg t)
  | `Call (call_id, `Editor_msg t) ->
      Ojsed_server.handle_call ~rights root rpc call_id (`Editor_msg t)
  | _ -> failwith "Unhandled message"

let () = connections#set_handle_message handle_message

let handle_con uri (stream, push) =
  connections#add_connect stream push
(*
let handle_con root uri (stream, push) =
  let root = Ojs_path.of_string root in
  let root =
    if Ojs_path.is_absolute root
    then root
    else Ojs_path.normalize (Ojs_path.append_path (Ojs_path.of_string (Sys.getcwd())) root)
  in
  let rpc_handler =
    let send msg =
      let wsdata = wsdata_of_msg msg in
      let frame = Websocket.Frame.of_string wsdata in
      Lwt.return (push (Some frame))
    in
    Ojs_rpc.rpc_handler send
  in
  let handle_message push_msg msg =
    match msg with
      `Filetree_msg t -> Ojsft_server.handle_message ~filepred root push_msg (`Filetree_msg t)
    | `Editor_msg t -> Ojsed_server.handle_message ~rights root push_msg (`Editor_msg t)
    | `Call (call_id, `Editor_msg t) ->
        Ojsed_server.handle_call ~rights root rpc_handler call_id (`Editor_msg t)
    | _ -> failwith "Unhandled message"
  in
  Ojs_server.handle_messages
    msg_of_wsdata wsdata_of_msg
    handle_message stream push
*)

let server sockaddr = Websocket.establish_server sockaddr handle_con

let run_server host port =
  Lwt_io_ext.sockaddr_of_dns host (string_of_int port) >>= fun sa ->
    Lwt.return (server sa) >>= fun _ -> wait_forever ()

let _ = Lwt_unix.run (run_server "0.0.0.0" 8080)
