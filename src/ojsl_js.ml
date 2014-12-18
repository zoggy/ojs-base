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

open Ojs_js

let (>>=) = Lwt.(>>=)

let log = Ojs_js.log

module type P = sig
    include Ojsl_types.P
    val insert : Ojs_js.id -> elt -> Ojs_js.id
  end

module Make (P : P) =
  struct
    class ['a] elist call send ~msg_id id =
      object(self)
        val mutable list = ([] : (string * 'a) list)

        method id : Ojs_js.id = id
        method msg_id : Ojs_js.id = msg_id
        method display_error msg = Ojsmsg_js.display_text_error msg_id msg
        method simple_call : 'a P.client_msg -> unit Lwt.t = fun msg ->
          call msg
            (fun msg -> Lwt.return
               (match msg with
                | P.SError msg -> self#display_error msg
                | _ -> ()
               )
            )

        method handle_add (e: 'a) =
          let new_id = P.insert id e in
          list <- (new_id, e) :: list

        method handle_delete (e: 'a) =
          try
            let (elt_id,_) = List.find (fun (id, elt) -> e = elt) list in
            let parent_node = Ojs_js.node_by_id id in
            (match Ojs_js.node_by_id elt_id with
             | exception _ -> ()
             | node ->
                 ignore(parent_node##removeChild((node :> Dom.node Js.t)))
            );
            list <- List.filter (fun (_,elt) -> elt <> e) list
          with Not_found -> ()

        method set_list (l: 'a list) =
          (*log (Printf.sprintf "setting list len=%d, id=%s" (List.length l) id);*)
          let parent_node = Ojs_js.node_by_id id in
          log "clearing children ";
          Ojs_js.clear_children parent_node ;
          list <- [];
          List.iter self#handle_add l

        method handle_message (msg : 'a P.server_msg) =
        try
          (match msg with
             | P.SList l -> self#set_list l
             | P.SAdd e -> self#handle_add e
             | P.SDelete e -> self#handle_delete e
             | P.SOk -> ()
             | P.SError msg -> self#display_error msg
             | _ -> failwith "Unhandled message received from server"
            );
            Js._false
          with
            e ->
              log (Printexc.to_string e);
              Js._false

        method update_list : unit Lwt.t =
          call P.Get
            (function
             | P.SList l -> Lwt.return (self#set_list l)
             | P.SError msg -> Lwt.return(self#display_error msg)
             | _ -> Lwt.return_unit)

        initializer
          ignore(self#update_list)
      end

    class ['a] elists
      (call : P.app_client_msg ->
              (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t)
        (send : P.app_client_msg -> unit)
        spawn
(*  (spawn : ('clt -> ('srv -> unit Lwt.t) -> unit Lwt.t) ->
           ('clt -> unit) ->
           msg_id: string -> string -> ('clt, 'srv) tree) *)=
      object(self)
        val mutable lists = (SMap.empty : 'a elist SMap.t)

        method get_list id =
          try SMap.find id lists
          with Not_found -> failwith ("No list "^id)

        method get_msg_id id = (self#get_list id)#msg_id

        method setup_list ~(msg_id:string) (id : string) =
          let send msg = send (P.pack_client_msg id msg) in
          let call msg cb =
            let cb msg =
              match P.unpack_server_msg msg with
              | Some (_, msg) -> cb msg
              | None -> Lwt.return_unit
            in
            call (P.pack_client_msg id msg) cb
          in
          let l = spawn call send ~msg_id id in
          lists <- SMap.add id l lists;
          l

        method handle_message msg =
          match P.unpack_server_msg msg with
          | Some (id, msg) ->
              let l = self#get_list id in
              l#handle_message msg
          | None -> Js._false

      end
  end
