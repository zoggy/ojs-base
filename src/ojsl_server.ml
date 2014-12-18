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

(** Handling lists on server side. *)

open Ojs_server
open Ojsl_types
let (>>=) = Lwt.(>>=)

module Make (P : Ojsl_types.P) =
  struct
    class ['a] elist
      (broadcall : 'a P.server_msg -> ('a P.client_msg -> unit Lwt.t) -> unit Lwt.t)
        (broadcast : 'a P.server_msg -> unit Lwt.t) ~id init =
        object(self)
          val mutable list = (init : 'a list)
          method list = list
          method set_list l = list <- l ; broadcast (P.SList l)

          method id = (id : string)

          method handle_add reply x =
            list <- x :: list;
            reply P.SOk >>= fun _ -> broadcast (P.SAdd x)

          method handle_delete reply x =
            list <- List.filter ((<>) x) list;
            reply P.SOk >>= fun _ -> broadcast (P.SAdd x)

          method handle_get reply = reply (P.SList list)

          method handle_message (send_msg : 'a P.server_msg -> unit Lwt.t) (msg : 'a P.client_msg) =
            self#handle_call send_msg msg

          method handle_call (reply_msg : 'a P.server_msg -> unit Lwt.t) (msg : 'a P.client_msg) =
            match msg with
            | P.Get -> self#handle_get reply_msg
            | P.Add x -> self#handle_add reply_msg x
            | P.Delete x -> self#handle_delete reply_msg x
            | _ -> failwith "List: Unhandled message"

        end

    class ['a] elists broadcall broadcast spawn
(*      (broadcall : (string * 'a P.server_msg) P.msg ->
         ((string * 'a P.client_msg) P.msg -> unit Lwt.t) -> unit Lwt.t)
        (broadcast : (string * 'a P.server_msg) P.msg -> unit Lwt.t)
        (spawn : ('a P.server_msg -> ('a P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
         ('a P.server_msg -> unit Lwt.t) ->
           id: string -> 'a list -> 'a elist
        )*)
        =
        object(self)
          val mutable lists = (SMap.empty : 'a elist SMap.t)

          method list id =
            try SMap.find id lists
            with Not_found -> failwith (Printf.sprintf "No list with id %S" id)

          method add_list ~id (init : 'a list) =
            let broadcall msg cb =
              let cb msg =
                match P.unpack_msg msg with
                | Some (_, msg) -> cb msg
                | None -> Lwt.return_unit
              in
              broadcall (P.pack_msg id msg) cb
            in
            let broadcast msg = broadcast (P.pack_msg id msg) in
            let elist = spawn broadcall broadcast ~id init in
            lists <- SMap.add id elist lists;
            elist

          method handle_message
            (send_msg : (string * 'a P.server_msg) P.msg -> unit Lwt.t)
              (msg : (string * 'a P.client_msg) P.msg) =
              match P.unpack_msg msg with
              | Some (id, msg) ->
                  let send_msg msg = send_msg (P.pack_msg id msg) in
                  (self#list id)#handle_message send_msg msg
              | None -> Lwt.return_unit

          method handle_call
            (return : (string * 'a P.server_msg) P.msg -> unit Lwt.t)
              (msg : (string * 'a P.client_msg) P.msg) =
              match P.unpack_msg msg with
              | Some (id, msg) ->
                  let reply_msg msg = return (P.pack_msg id msg) in
                  (self#list id)#handle_call reply_msg msg
              | None -> Lwt.return_unit
        end
  end



  