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


let wsdata_of_msg msg = J.to_string (Ojsed_types.server_msg_to_yojson msg)
let msg_of_wsdata s = Ojs_server.mk_msg_of_wsdata Ojsed_types.client_msg_of_yojson

let access_forbidden path = `Error ("Forbidden access to "^(Ojs_path.to_string path))

class ['clt, 'srv] editor
  (broadcall : 'srv -> ('clt -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : 'srv -> unit Lwt.t) ~id root =
    object(self)
      method id = (id : string)
      method root = (root : Ojs_path.t)

      method can_read_file file = true
      method can_write_file file = true

      method handle_get_file_contents reply_msg path =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string norm in
        match self#can_read_file file with
        | false -> reply_msg (access_forbidden path)
        | true ->
            let contents = Ojsed_files.string_of_file file in
            reply_msg (`File_contents (path, contents))

      method handle_save_file reply_msg path contents =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string norm in
        match self#can_write_file file with
        | false -> reply_msg (access_forbidden path)
        | true ->
            Ojsed_files.file_of_string ~file contents ;
            reply_msg (`Ok (Printf.sprintf "File %S saved" (Ojs_path.to_string path)))

      method handle_message (send_msg : 'srv -> unit Lwt.t) (msg : 'clt) =
        self#handle_call send_msg msg

      method handle_call (reply_msg : 'srv -> unit Lwt.t) (msg : 'clt) =
        match msg with
        |  `Get_file_contents path ->
            self#handle_get_file_contents reply_msg path
        | `Save_file (path, contents) ->
            self#handle_save_file reply_msg path contents
        | _ ->
            reply_msg (`Error "Unhandled message")
      end

class ['clt, 'srv] editors
  (broadcall : [>'srv Ojsed_types.msg] -> ([>'clt Ojsed_types.msg] -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : [>'srv Ojsed_types.msg] -> unit Lwt.t)
    (spawn : ('src -> ('clt -> unit Lwt.t) -> unit Lwt.t) ->
     ('srv -> unit Lwt.t) ->
       id: string -> Ojs_path.t -> ('clt, 'srv) editor
    )
    =
    object(self)
      val mutable editors = (SMap.empty : ('clt, 'srv) editor SMap.t)

      method editor id =
        try SMap.find id editors
        with Not_found -> failwith (Printf.sprintf "No editor with id %S" id)

      method add_editor ~id root =
        let broadcall msg cb =
          let cb = function
            `Editor_msg (_, msg) -> cb msg
          | _ -> Lwt.return_unit
          in
          broadcall (`Editor_msg (id, msg)) cb
        in
        let broadcast msg = broadcast  (`Editor_msg (id, msg)) in
        let ed = spawn broadcall broadcast ~id root in
        editors <- SMap.add id ed editors;
        ed

      method handle_message
        (send_msg : 'srv Ojsed_types.msg -> unit Lwt.t) (msg : 'clt Ojsed_types.msg) =
          match msg with
            `Editor_msg (id, msg) ->
              let send_msg msg = send_msg (`Editor_msg (id, msg)) in
              (self#editor id)#handle_message send_msg msg

      method handle_call (return : 'srv Ojsed_types.msg -> unit Lwt.t) (msg : 'clt Ojsed_types.msg) =
        match msg with
          `Editor_msg (id, msg) ->
            let reply_msg msg = return (`Editor_msg (id, msg)) in
            (self#editor id)#handle_call reply_msg msg
  end
