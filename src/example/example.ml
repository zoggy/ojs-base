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

let file_filter =
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


module Server_P = struct
  include Ojs_rpc.Base(Example_types.App_msg)
  let wsdata_of_msg msg = J.to_string (Example_types.server_msg_to_yojson msg)
  let msg_of_wsdata = Ojs_server.mk_msg_of_wsdata Example_types.client_msg_of_yojson
  end
module Server = Ojs_server.Make(Server_P)
module SFT = Ojsft_server.Make(Example_types.FT)
module SED = Ojsed_server.Make(Example_types.ED)

module PList = Example_types.PList
module SMylist = Ojsl_server.Make(PList)

class mylist
  broadcall broadcast ~id init =
   object(self)
     inherit [int] SMylist.elist broadcall broadcast ~id init as super
     method private handle_clear reply =
       list <- [];
       reply PList.SOk >>= fun _ -> broadcast (PList.SUpdate [])

     method handle_call reply_msg = function
       PList.Clear -> self#handle_clear reply_msg
     | msg -> super#handle_call reply_msg msg
  end


let connections = new Server.connection_group
let filetrees = new SFT.filetrees connections#broadcall connections#broadcast
  (new SFT.filetree)
let editors = new SED.editors connections#broadcall connections#broadcast
  (new SED.editor)

let lists = new SMylist.elists connections#broadcall connections#broadcast
  (new mylist)

let root =
  let root = try Sys.argv.(1) with _ -> "." in
  let root = Ojs_path.of_string root in
  if Ojs_path.is_absolute root
  then root
  else Ojs_path.normalize (Ojs_path.append_path (Ojs_path.of_string (Sys.getcwd())) root)

let ft = filetrees#add_filetree "ft" root
let () = ft#set_file_filter file_filter
let _ = editors#add_editor "ed" root
let list = lists#add_list "elist" [1 ; 2 ; 3]

let handle_message send_msg rpc msg =
    match msg with
    | Example_types.ED.Editor _ -> editors#handle_message send_msg msg
    | Example_types.FT.Filetree _ -> filetrees#handle_message  send_msg msg
    | PList.Mylist _ -> lists#handle_message send_msg msg
    | Server_P.Call (call_id, ((Example_types.FT.Filetree _) as msg))->
        let return msg = Server.Rpc.return rpc call_id msg in
        filetrees#handle_call return msg
    | Server_P.Call (call_id, ((Example_types.ED.Editor _) as msg)) ->
        let return msg = Server.Rpc.return rpc call_id msg in
        editors#handle_call return msg
    | Server_P.Call (call_id, ((PList.Mylist _) as msg)) ->
        let return msg = Server.Rpc.return rpc call_id msg in
        lists#handle_call return msg

    | _ -> failwith "Unhandled message"

let () = connections#set_handle_message handle_message

let handle_con id req recv push =
  let stream = Websocket_lwt.mk_frame_stream recv in
  connections#add_connection stream push
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


let buffer =  Buffer.create 256
let () = Lwt_log.render ~buffer ~template: "$(message)" ~section:
  Websocket_lwt.section ~level:Lwt_log_core.Debug ~message:"coucou"

let run_server host port =
  let uri = Uri.of_string (Printf.sprintf "http://%s:%d/" host port) in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
    let ctx = Conduit_lwt_unix.default_ctx in
    Conduit_lwt_unix.endp_to_server ~ctx endp >>= fun server ->
  let server =
    Websocket_lwt.establish_standard_server ~ctx ~mode:server handle_con
  in
  ignore @@ server ; fst @@ Lwt.wait ()


let _ = Lwt_main.run (run_server "0.0.0.0" 8080)
