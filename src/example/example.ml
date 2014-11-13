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
let msg_of_wsdata s =
  try
    let json = J.from_string s in
    match Example_types.client_msg_of_yojson json with
      `Error s -> raise (Yojson.Json_error s)
    | `Ok msg -> Some msg
  with
    Yojson.Json_error s ->
      prerr_endline s;
      None
  | e ->
      prerr_endline (Printexc.to_string e);
      None

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

let handle_con root uri (stream, push) =
  let root = if Filename.is_relative root
    then Ojs_misc.normalize_filename (Filename.concat (Sys.getcwd()) root)
    else root
  in
  let handle_message push_msg msg =
    match msg with
      `Filetree_msg t -> Ojsft_server.handle_message ~filepred root push_msg (`Filetree_msg t)
    | `Editor_msg t -> Ojsed_server.handle_message ~rights root push_msg (`Editor_msg t)
    | _ -> failwith "Unhandled message"
  in
  Ojs_server.handle_messages
    msg_of_wsdata wsdata_of_msg
    handle_message stream push

let server root sockaddr = Websocket.establish_server sockaddr (handle_con root)

let run_server root host port =
  Lwt_io_ext.sockaddr_of_dns host (string_of_int port) >>= fun sa ->
    Lwt.return (server root sa) >>= fun _ -> wait_forever ()

let _ = Lwt_unix.run (run_server (try Sys.argv.(1) with _ -> ".") "0.0.0.0" 8080)
