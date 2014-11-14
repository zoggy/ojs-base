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

type access_right = [`RW | `RO ]


type behaviour = {
    rights : Ojs_path.t -> access_right option ;
    after_get_tree : Ojsft_types.file_tree list -> Ojsft_types.file_tree list ;
    before_add_file : Ojs_path.t -> unit ;
    after_add_file : Ojs_path.t -> unit ;
  }

let default_behaviour = {
    rights = (fun _ -> Some `RW);
    after_get_tree = (fun x -> x) ;
    before_add_file = (fun _ -> ()) ;
    after_add_file = (fun _ -> ()) ;
  }

let access_rights behav root path =
  let path = Ojs_path.append_path root path in
  let norm = Ojs_path.normalize path in
  (*prerr_endline ("norm="^norm);*)
  if Ojs_path.is_prefix root norm then
    (norm, behav.rights norm)
  else
    (norm, None)

let access_forbidden path = `Error ("Forbidden access to "^(Ojs_path.to_string path))
let creation_forbidden path = `Error ("Forbidden creation of "^(Ojs_path.to_string path))

let wsdata_of_msg msg = J.to_string (Ojsft_types.server_msg_to_yojson msg)
let msg_of_wsdata s =
  try
    let json = J.from_string s in
    match Ojsft_types.client_msg_of_yojson json with
      `Error s -> raise (Yojson.Json_error s)
    | `Ok msg -> Some msg
  with
    Yojson.Json_error s ->
      prerr_endline s;
      None
  | e ->
      prerr_endline (Printexc.to_string e);
      None

let send_msg push_msg id msg = push_msg (`Filetree_msg (id, msg))

let handle_add_file behav root path contents =
  match access_rights behav root path with
  | (_, None)
  | (_, Some `RO) -> [ access_forbidden path ]
  | (norm, Some `RW) ->
      let file = Ojs_path.to_string norm in
      if not (Sys.file_exists file) &&
        snd (access_rights behav root (Ojs_path.parent path)) <> Some `RW
      then
       [ creation_forbidden path ]
      else
        begin
          behav.before_add_file norm ;
          Ojs_misc.file_of_string ~file contents ;
          behav.after_add_file norm ;
          [`Add_file path]
        end

let handle_client_msg ?filepred behav root id msg =
  match msg with
    `Get_tree ->
      let files = Ojsft_files.file_trees_of_dir ?filepred root in
      let files = behav.after_get_tree files in
      (id, [`Tree files])
  | `Add_file (path, contents) ->
      prerr_endline ("Add_file "^(Ojs_path.to_string path)^"\n"^contents);
      let res = handle_add_file behav root path contents in
      (id, res)
  | _ ->
      failwith "Unhandled message"

let send_messages push_msg (id, messages) =
  Lwt_list.iter_s (send_msg push_msg id) messages

let handle_message ?filepred ?(behav=default_behaviour) root push_msg msg =
  try
    match msg with
    | `Filetree_msg (id, t) ->
        Lwt.catch
          (fun () -> send_messages push_msg
             (handle_client_msg ?filepred behav root id t)
          )
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

