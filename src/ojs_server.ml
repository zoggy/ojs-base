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

let (>>=) = Lwt.bind

module J = Yojson.Safe

let handle_messages msg_of_wsdata wsdata_of_msg
  handle_message stream push =
  let push_msg msg =
    let wsdata = wsdata_of_msg msg in
    let frame = Websocket.Frame.of_string wsdata in
    Lwt.return (push (Some frame))
  in
  let f frame =
    let s = Websocket.Frame.content frame in
    try
      match msg_of_wsdata s with
        None -> Lwt.return_unit
      | Some msg -> handle_message push_msg msg
    with
    | e ->
        Lwt.return (prerr_endline (Printexc.to_string e))
  in
  Lwt.catch
    (fun _ -> Lwt_stream.iter_s f stream)
    (fun _ -> Lwt.return_unit)

