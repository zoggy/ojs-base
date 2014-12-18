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

module Make (P : Ojsl_types.P) =
  struct
    class ['a] elist call send ~msg_id id =
      object(self)
        method id : string = id
        method msg_id : string = msg_id
        method display_error msg = Ojsmsg_js.display_text_error msg_id msg
        method simple_call : 'a P.client_msg -> unit Lwt.t = fun msg ->
          call msg
            (fun msg -> Lwt.return
               (match msg with
                | P.SError msg -> self#display_error msg
                | _ -> ()
               )
            )
        method handle_add (e: 'a) = ()
        method handle_delete (e: 'a) = ()
        method set_list (l: 'a list) = ()

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

  end
