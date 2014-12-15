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

let access_forbidden path = `Error ("Forbidden access to "^(Ojs_path.to_string path))
let creation_forbidden path = `Error ("Forbidden creation of "^(Ojs_path.to_string path))
let deletion_forbidden path = `Error ("Forbidden deletion of "^(Ojs_path.to_string path))

let wsdata_of_msg msg = J.to_string (Ojsft_types.server_msg_to_yojson msg)
let msg_of_wsdata s = Ojs_server.mk_msg_of_wsdata Ojsft_types.client_msg_of_yojson

class ['clt, 'srv] filetree
  (broadcall : 'srv -> ('clt -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : 'srv -> unit Lwt.t) ~id root =
    object(self)
      val mutable file_filter = (fun (_:Ojs_path.t) -> true)
      method set_file_filter f = file_filter <- f

      method id : string = id
      method root : Ojs_path.t = root

      method can_add_file file = true
      method can_add_dir (dir : string) = true
      method can_delete file = true


      method before_add_file (filename : Ojs_path.t) = ()
      method after_add_file (filename : Ojs_path.t) = ()
      method handle_add_file reply_msg path contents =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string norm in
        match self#can_add_file file with
          false -> reply_msg (creation_forbidden path)
        | true ->
            let contents =
              try Base64.decode contents
              with e -> failwith (Printexc.to_string e)
            in
            self#before_add_file norm ;
            Ojs_misc.file_of_string ~file contents ;
            self#after_add_file norm ;
            reply_msg `Ok >>=
            fun () -> broadcast (`Add_file path)

      method handle_add_dir reply_msg path =
        let norm = Ojs_path.normalize path in
        let dir = Ojs_path.to_string norm in
        match self#can_add_dir dir with
        | false -> reply_msg (creation_forbidden path)
        | true ->
            try
              Unix.mkdir dir 0o755 ;
              reply_msg `Ok >>=
                fun () -> broadcast (`Add_dir path)
            with Unix.Unix_error (e, s1, s2) ->
                let msg = Printf.sprintf "Could not create %s: %s"
                  (Ojs_path.to_string path) (Unix.error_message e)
                in
                reply_msg (`Error msg)

      method handle_delete reply_msg path =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string norm in
        match self#can_delete file with
        | false -> reply_msg (deletion_forbidden path)
        | true ->
            if Sys.is_directory file then
              try Sys.remove file; reply_msg `Ok
              with Sys_error msg -> failwith msg
            else
              match Sys.command (Printf.sprintf "rm -fr %s" (Filename.quote file)) with
                0 -> reply_msg `Ok
              | n ->
                  let msg = Printf.sprintf "Could not delete %s" (Ojs_path.to_string path) in
                  reply_msg (`Error msg)

      method handle_rename reply_msg path1 path2 =
        reply_msg (`Error "Rename: Not implemented")

      method after_get_tree files = files

      method handle_message (send_msg : 'srv -> unit Lwt.t) (msg : 'clt) =
        self#handle_call send_msg msg

      method handle_call (reply_msg : 'srv -> unit Lwt.t) (msg : 'clt) =
        match msg with
          `Get_tree ->
            let files = Ojsft_files.file_trees_of_dir ~filepred: file_filter root in
            let files = self#after_get_tree files in
            reply_msg (`Tree files)
        | `Add_file (path, contents) ->
            self#handle_add_file reply_msg path contents
        | `Add_dir path ->
            self#handle_add_dir reply_msg path
        | `Delete path ->
            self#handle_delete reply_msg path
        | `Rename (path1, path2) ->
            self#handle_rename reply_msg path1 path2
        | _ ->
            reply_msg (`Error "Unhandled message")

    end

class ['clt, 'srv] filetrees
   (broadcall : [>'srv Ojsft_types.msg] -> ([>'clt Ojsft_types.msg] -> unit Lwt.t) -> unit Lwt.t)
   (broadcast : [>'srv Ojsft_types.msg] -> unit Lwt.t)
   (spawn : ('src -> ('clt -> unit Lwt.t) -> unit Lwt.t) ->
            ('srv -> unit Lwt.t) ->
            id: string -> Ojs_path.t -> ('clt, 'srv) filetree
   )
   =
  object(self)
    val mutable filetrees = (SMap.empty : ('clt, 'srv) filetree SMap.t)

    method filetree id =
      try SMap.find id filetrees
      with Not_found -> failwith (Printf.sprintf "No filetree with id %S" id)

    method add_filetree ~id root =
        let broadcall msg cb =
          let cb = function
            `Filetree_msg (_, msg) -> cb msg
          | _ -> Lwt.return_unit
          in
          broadcall (`Filetree_msg (id, msg)) cb
        in
        let broadcast msg = broadcast  (`Filetree_msg (id, msg)) in
        let ft = spawn broadcall broadcast ~id root in
        filetrees <- SMap.add id ft filetrees;
        ft

      method handle_message
        (send_msg : 'srv Ojsft_types.msg -> unit Lwt.t) (msg : 'clt Ojsft_types.msg) =
          match msg with
            `Filetree_msg (id, msg) ->
              let send_msg msg = send_msg (`Filetree_msg (id, msg)) in
              (self#filetree id)#handle_message send_msg msg

      method handle_call (return : 'srv Ojsft_types.msg -> unit Lwt.t) (msg : 'clt Ojsft_types.msg) =
        match msg with
          `Filetree_msg (id, msg) ->
            let reply_msg msg = return (`Filetree_msg (id, msg)) in
            (self#filetree id)#handle_call reply_msg msg
  end