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
module SMap = Map.Make(String)
module Ws = Websocket_lwt

let mk_msg_of_wsdata client_msg_of_yojson =
  fun s ->
    try
      let json = J.from_string s in
      match client_msg_of_yojson json with
        Error s -> raise (Yojson.Json_error s)
      | Ok msg -> Some msg
    with
      Yojson.Json_error s ->
        prerr_endline s;
        None
    | e ->
        prerr_endline (Printexc.to_string e);
        None

let mk_send_msg wsdata_of_msg push =
  fun msg ->
    let wsdata = wsdata_of_msg msg in
    let frame = Ws.Frame.create ~content: wsdata () in
    push frame

let mk_msg_stream msg_of_wsdata =
  let f frame =
    match Ws.Frame.(frame.content, frame.opcode) with
    | content, Ws.Frame.Opcode.Text -> msg_of_wsdata content
    | _ -> None
  in
  Lwt_stream.filter_map f

let handle_messages msg_of_wsdata wsdata_of_msg handle_message stream push =
  let push_msg = mk_send_msg wsdata_of_msg push in
  let f msg =
    try handle_message push_msg
    with e ->
        Lwt.return (prerr_endline (Printexc.to_string e))
  in
  Lwt.catch
    (fun _ -> Lwt_stream.iter_s f (mk_msg_stream msg_of_wsdata stream))
    (fun _ -> Lwt.return_unit)

module type P =
  sig
    include Ojs_rpc.B
    val msg_of_wsdata : string -> app_client_msg option
    val wsdata_of_msg : app_server_msg -> string
  end


module type S = sig
  module Rpc : Ojs_rpc.S
  class connection_group :
    object
      val mutable connections :
        ((Rpc.app_server_msg -> unit Lwt.t) * Rpc.t) list
      val mutable handle_message :
        (Rpc.app_server_msg -> unit Lwt.t) ->
        Rpc.t -> Rpc.app_client_msg -> unit Lwt.t
      method add_connection :
        Ws.Frame.t Lwt_stream.t ->
        (Ws.Frame.t -> unit Lwt.t) -> unit Lwt.t
      method broadcall :
        Rpc.app_server_msg ->
        (Rpc.app_client_msg -> unit Lwt.t) -> unit Lwt.t
      method broadcast : Rpc.app_server_msg -> unit Lwt.t
      method handle_message :
        (Rpc.app_server_msg -> unit Lwt.t) ->
        Rpc.t -> Rpc.app_client_msg -> unit Lwt.t
      method remove_connection :
        (Rpc.app_server_msg -> unit Lwt.t) -> unit
      method set_handle_message :
        ((Rpc.app_server_msg -> unit Lwt.t) ->
         Rpc.t -> Rpc.app_client_msg -> unit Lwt.t) ->
        unit
    end
end

module Make(P:P) =
  struct
    module Rpc = Ojs_rpc.Make_server(P)
    class connection_group =
      object(self)
        val mutable handle_message = (fun _ _ _ -> Lwt.return_unit)
        val mutable connections =
          ([] : ((P.app_server_msg -> unit Lwt.t) * Rpc.t) list)

      method remove_connection send =
        let pred (send2, _) = send <> send2 in
        connections <- List.filter pred connections

      method add_connection stream push =
        let send_msg = mk_send_msg P.wsdata_of_msg push in
        let rpc = Rpc.rpc_handler send_msg in
        connections <- (send_msg, rpc) :: connections;
        let stream = mk_msg_stream P.msg_of_wsdata stream in
        Lwt.catch
          (fun _ -> Lwt_stream.iter_s
               (fun msg ->
                  Lwt.catch
                    (fun () -> self#handle_message send_msg rpc msg)
                    (fun e ->
                       prerr_endline (Printexc.to_string e);
                       Lwt.return_unit)
               )
                 stream
            )
          (fun e ->
             prerr_endline (Printexc.to_string e);
             Lwt.return_unit)

      method broadcast msg =
        let f (send, _) =
          Lwt.catch
            (fun _ -> send msg)
            (fun _ -> self#remove_connection send; Lwt.return_unit)
        in
        Lwt_list.iter_s f connections

      method broadcall (msg : 'srv) (cb : 'clt -> unit Lwt.t) =
        let f (send, rpc) =
          Lwt.catch
            (fun _ -> Rpc.call rpc msg cb)
            (fun _ -> self#remove_connection send; Lwt.return_unit)
        in
        Lwt_list.iter_s f connections

      method handle_message :
        (P.app_server_msg -> unit Lwt.t) -> Rpc.t -> P.app_client_msg -> unit Lwt.t =
          handle_message
      method set_handle_message f = handle_message <- f
  end
end