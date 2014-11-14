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

open Ojs_server

type access_right = [`RW | `RO]

let wsdata_of_msg msg = J.to_string (Ojsed_types.server_msg_to_yojson msg)
let msg_of_wsdata s =
  try
    let json = J.from_string s in
    match Ojsed_types.client_msg_of_yojson json with
      `Error s -> raise (Yojson.Json_error s)
    | `Ok msg -> Some msg
  with
    Yojson.Json_error s ->
      prerr_endline s;
      None
  | e ->
      prerr_endline (Printexc.to_string e);
      None

let send_msg push_msg id msg = push_msg (`Editor_msg (id, msg))

let access_rights ?(rights=fun _ -> Some `RW) root path =
  let path = Ojs_path.append_path root path in
  let norm = Ojs_path.normalize path in
  (*prerr_endline ("norm="^norm);*)
  if Ojs_path.is_prefix root norm then
    (norm, rights norm)
  else
    (norm, None)

let access_forbidden path = `Error ("Forbidden access to "^(Ojs_path.to_string path))

let handle_client_msg ?rights root id msg =
  match msg with
  |  `Get_file_contents path ->
      begin
        match access_rights ?rights root path with
        | (_, None) -> (id, [access_forbidden path])
        | (file, Some `RW)
        | (file, Some `RO) ->
            let contents = Ojsed_files.string_of_file (Ojs_path.to_string file) in
            (id, [`File_contents (path, contents)])
      end
  | `Save_file (path, contents) ->
      begin
        match access_rights ?rights root path with
        | (_, None)
        | (_, Some `RO) -> (id, [access_forbidden path])
        | (file, Some `RW) ->
            let file = Ojs_path.to_string file in
            Ojsed_files.file_of_string ~file contents ;
            (id, [`Ok (Printf.sprintf "File %S saved" (Ojs_path.to_string path))])
      end
  | _ ->
      failwith "Unhandled message"

let send_messages push_msg (id, messages) =
  Lwt_list.iter_s (send_msg push_msg id) messages

let handle_message ?rights root push_msg msg =
  try
    match msg with
    | `Editor_msg (id, t) ->
        Lwt.catch
          (fun () ->
             send_messages push_msg
               (handle_client_msg ?rights root id t))
          (fun e ->
             let msg =
               match e with
                 Failure s | Sys_error s -> s
               | _ -> Printexc.to_string e
             in
             send_msg push_msg id (`Error msg)
          )
  with
  | e ->
      Lwt.return (prerr_endline (Printexc.to_string e))

