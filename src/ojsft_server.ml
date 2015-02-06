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

module Make(P:Ojsft_types.P) =
  struct

let access_forbidden path =
  P.SError (Printf.sprintf "Forbidden access to %S" (Ojs_path.to_string path))
let creation_forbidden path =
  P.SError (Printf.sprintf "Forbidden creation of %S" (Ojs_path.to_string path))
let deletion_forbidden path =
  P.SError (Printf.sprintf "Forbidden deletion of %S " (Ojs_path.to_string path))
let renaming_forbidden path1 path2 =
  P.SError (Printf.sprintf "Forbidden renaming of %S to %S"
   (Ojs_path.to_string path1) (Ojs_path.to_string path2))

class filetree
  (broadcall : P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : P.server_msg -> unit Lwt.t) ~id root =
    object(self)
      val mutable file_filter = (fun (_:Ojs_path.t) -> true)
      method set_file_filter f = file_filter <- f

      method id : string = id
      method root : Ojs_path.t = root

      method can_add_file file = true
      method can_add_dir dir = true
      method can_delete file = true
      method can_rename file1 file2 = true

      method before_add_file (filename : Ojs_path.t) = ()
      method after_add_file (filename : Ojs_path.t) = ()
      method handle_add_file reply_msg path contents =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string (Ojs_path.append_path root norm) in
        match self#can_add_file file with
          false -> reply_msg (creation_forbidden path)
        | true ->
            let contents =
              try B64.decode contents
              with e -> failwith (Printexc.to_string e)
            in
            self#before_add_file norm ;
            Ojs_misc.file_of_string ~file contents ;
            self#after_add_file norm ;
            reply_msg P.SOk >>=
            fun () -> broadcast (P.SAdd_file path)

      method handle_add_dir reply_msg path =
        let norm = Ojs_path.normalize path in
        let dir = Ojs_path.to_string (Ojs_path.append_path root norm) in
        match self#can_add_dir dir with
        | false -> reply_msg (creation_forbidden path)
        | true ->
            try
              Unix.mkdir dir 0o755 ;
              reply_msg P.SOk >>=
                fun () -> broadcast (P.SAdd_dir path)
            with Unix.Unix_error (e, s1, s2) ->
                let msg = Printf.sprintf "Could not create %s: %s"
                  (Ojs_path.to_string path) (Unix.error_message e)
                in
                reply_msg (P.SError msg)

      method handle_delete reply_msg path =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string (Ojs_path.append_path root norm) in
        prerr_endline ("handle_delete, file="^file);
        match self#can_delete file with
        | false -> reply_msg (deletion_forbidden path)
        | true ->
            if not (Sys.is_directory file) then
              try
                Sys.remove file;
                reply_msg P.SOk >>= fun () ->
                  broadcast (P.SDelete path)
              with Sys_error msg -> failwith msg
            else
              match Sys.command (Printf.sprintf "rm -fr %s" (Filename.quote file)) with
                0 ->
                  reply_msg P.SOk >>= fun () ->
                    broadcast (P.SDelete path)
              | n ->
                  let msg = Printf.sprintf "Could not delete %s" (Ojs_path.to_string path) in
                  reply_msg (P.SError msg)

      method handle_rename reply_msg path1 path2 =
        let norm1 = Ojs_path.normalize path1 in
        let file1 = Ojs_path.to_string (Ojs_path.append_path root norm1) in
        let norm2 = Ojs_path.normalize path2 in
        let file2 = Ojs_path.to_string (Ojs_path.append_path root norm2) in
        match self#can_rename file1 file2 with
          false -> reply_msg (renaming_forbidden path1 path2)
        | true ->
            try
              Sys.rename file1 file2;
              reply_msg P.SOk
              >>= fun () ->
                broadcast (P.SDelete path1) >>= fun () ->
                if Sys.is_directory file2 then
                  broadcast (P.SAdd_dir path2)
                else
                  broadcast (P.SAdd_file path2)
            with Sys_error msg ->
                let msg = Printf.sprintf "Could not rename %S to %S: %s"
                  (Ojs_path.to_string path1) (Ojs_path.to_string path2) msg
                in
                reply_msg (P.SError msg)

      method after_get_tree files = files

      method handle_message (send_msg : 'srv -> unit Lwt.t) (msg : 'clt) =
        self#handle_call send_msg msg

      method handle_call (reply_msg : 'srv -> unit Lwt.t) (msg : 'clt) =
        match msg with
          P.Get_tree ->
            let files = Ojsft_files.file_trees_of_dir ~filepred: file_filter root in
            let files = self#after_get_tree files in
            reply_msg (P.STree files)
        | P.Add_file (path, contents) ->
            self#handle_add_file reply_msg path contents
        | P.Add_dir path ->
            self#handle_add_dir reply_msg path
        | P.Delete path ->
            self#handle_delete reply_msg path
        | P.Rename (path1, path2) ->
            self#handle_rename reply_msg path1 path2
        | _ ->
            reply_msg (P.SError "Unhandled message")

    end

class filetrees
   (broadcall : P.app_server_msg -> (P.app_client_msg -> unit Lwt.t) -> unit Lwt.t)
   (broadcast : P.app_server_msg -> unit Lwt.t)
   (spawn : (P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
            (P.server_msg -> unit Lwt.t) ->
            id: string -> Ojs_path.t -> filetree
   )
   =
    object(self)
      val mutable filetrees = (SMap.empty : filetree SMap.t)

      method filetree id =
        try SMap.find id filetrees
        with Not_found -> failwith (Printf.sprintf "No filetree with id %S" id)

      method add_filetree ~id root =
        let broadcall msg cb =
          let cb msg =
            match P.unpack_client_msg msg with
            | Some (_, msg) -> cb msg
            | _ -> Lwt.return_unit
          in
          broadcall (P.pack_server_msg id msg) cb
        in
        let broadcast msg = broadcast (P.pack_server_msg id msg) in
        let ft = spawn broadcall broadcast ~id root in
        filetrees <- SMap.add id ft filetrees;
        ft

      method handle_message
        (send_msg : P.app_server_msg -> unit Lwt.t) (msg : P.app_client_msg) =
          match P.unpack_client_msg msg with
          | Some (id, msg) ->
              let send_msg msg = send_msg (P.pack_server_msg id msg) in
              (self#filetree id)#handle_message send_msg msg
          | None -> Lwt.return_unit

      method handle_call (return : P.app_server_msg -> unit Lwt.t) (msg : P.app_client_msg) =
        match P.unpack_client_msg msg with
        | Some (id, msg) ->
            let reply_msg msg = return (P.pack_server_msg id msg) in
            (self#filetree id)#handle_call reply_msg msg
        | None -> Lwt.return_unit
  end
end