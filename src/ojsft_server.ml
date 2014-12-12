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
let deletion_forbidden path = `Error ("Forbidden deletion of "^(Ojs_path.to_string path))

let wsdata_of_msg msg = J.to_string (Ojsft_types.server_msg_to_yojson msg)
let msg_of_wsdata s = Ojs_server.mk_msg_of_wsdata Ojsft_types.client_msg_of_yojson

class ['clt, 'srv] filetree ?(behav=default_behaviour)
  (broadcall : 'srv -> ('clt -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : 'srv -> unit) ~id root =
    object(self)
      method id : string = id
      method root : Ojs_path.t = root

      method can_add_file file = true
      method can_add_dir (dir : string) = true


      method handle_add_file send_msg path contents =
        let norm = Ojs_path.normalize path in
        let file = Ojs_path.to_string norm in
        match self#can_add_file file with
          false -> send_msg (creation_forbidden path)
        | true ->
            let contents =
              try Base64.decode contents
              with e -> failwith (Printexc.to_string e)
            in
            behav.before_add_file norm ;
            Ojs_misc.file_of_string ~file contents ;
            behav.after_add_file norm ;
            send_msg `Ok ;
            broadcast (`Add_file path)
            (*
        match access_rights behav root path with
        | (_, None)
        | (_, Some `RO) -> [ access_forbidden path ]
        | (norm, Some `RW) ->
            let contents =
              try Base64.decode contents
              with e -> failwith (Printexc.to_string e)
            in
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
                 *)
                 (*
let handle_add_dir behav root path =
  let parent = Ojs_path.parent path in
  match access_rights behav root parent with
  | (_, None)
  | (_, Some `RO) -> [ creation_forbidden path ]
  | (_, Some `RW) ->
      match access_rights behav root path with
      | (_, None)
      | (_, Some `RO) -> [ creation_forbidden path ]
      | (norm, Some `RW) ->
          let dir = Ojs_path.to_string norm in
          try
            Unix.mkdir dir 0o755 ;
            [ `Add_dir path ]
          with Unix.Unix_error (e, s1, s2) ->
             let msg = Printf.sprintf "Could not create %s: %s"
                (Ojs_path.to_string path) (Unix.error_message e)
              in
              failwith msg

let handle_delete behav root path =
  let parent = Ojs_path.parent path in
  match access_rights behav root parent with
  | (_, None)
  | (_, Some `RO) -> [ deletion_forbidden path ]
  | (_, Some `RW) ->
      match access_rights behav root path with
      | (_, None)
      | (_, Some `RO) -> [ deletion_forbidden path ]
      | (norm, Some `RW) ->
          let file = Ojs_path.to_string norm in
          if Sys.is_directory file then
            try Sys.remove file; [ `Ok ]
            with Sys_error msg -> failwith msg
          else
            match Sys.command (Printf.sprintf "rm -fr %s" (Filename.quote file)) with
              0 -> [ `Ok ]
            | n ->
                let msg = Printf.sprintf "Could not delete %s" (Ojs_path.to_string path) in
                failwith msg

let handle_rename behav root path1 path2 =
  failwith "Rename: Not implemented"
*)
      method handle_message (send_msg : 'srv -> unit) (msg : 'clt) =(*?filepred behav*)
        match msg with
          `Get_tree ->
            let files = Ojsft_files.file_trees_of_dir (*?filepred*) root in
            let files = behav.after_get_tree files in
            send_msg (`Tree files)
        | `Add_file (path, contents) ->
            self#handle_add_file send_msg path contents
(*        | `Add_dir path ->
            (id, handle_add_dir behav root path)
        | `Delete path ->
            (id, handle_delete behav root path)
        | `Rename (path1, path2) ->
            (id, handle_rename behav root path1 path2)*)
        | _ ->
            send_msg (`Error "Unhandled message")
(*
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
*)
    end

class ['clt, 'srv] filetrees broadcall broadcast =
  object(self)
    val mutable filetrees = (SMap.empty : ('clt, 'srv) filetree SMap.t)

    method filetree id =
      try SMap.find id filetrees
      with Not_found -> failwith (Printf.sprintf "No filetree with id %S" id)

    method add_filetree ~id root =
      let broadcall msg cb = broadcall (`Filetree_msg (id, msg)) cb in
      let broadcast msg = broadcast  (`Filetree_msg (id, msg)) in
      let ft = new filetree broadcall broadcast ~id root in
      filetrees <- SMap.add id ft filetrees

    method handle_message send_msg (msg : 'clt Ojsft_types.msg) =
      match msg with
        `Filetree_msg (id, msg) ->
          (self#filetree id)#handle_message send_msg msg
  end