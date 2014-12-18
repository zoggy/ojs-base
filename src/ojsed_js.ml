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

open Ojs_js
let (>>=) = Lwt.(>>=)

module Make(P:Ojsed_types.P) =
  struct
    class editor call (send : P.client_msg -> unit)
      ~bar_id ~msg_id ed_id =
    let editor = Ojs_ace.ace##edit (Js.string ed_id) in
    let _ = editor##setFontSize(Js.string "14px") in
    let bar = Ojs_js.node_by_id bar_id in
    let doc = Dom_html.document in
    let button = doc##createElement(Js.string "button") in
    let text = doc##createTextNode(Js.string "Save") in
    let filename_id = ed_id ^ "__filename" in
    let fname = doc##createElement(Js.string "span") in
    let _ =
      fname##setAttribute (Js.string "id", Js.string filename_id) ;
      fname##setAttribute (Js.string "class", Js.string "filename") ;
      Dom.appendChild bar button ;
      Dom.appendChild button text ;
      Dom.appendChild bar fname
    in
    object(self)
      val mutable current_file = (None : Ojsed_types.path option)
      val mutable sessions = (SMap.empty : Ojs_ace.editSession Js.t SMap.t)

      method id = ed_id
      method msg_id = msg_id

      method send_msg = send

      method get_session ?contents filename =
        let filename = Ojs_path.to_string filename in
        try  SMap.find filename sessions
        with Not_found ->
            let sess = Ojs_ace.newEditSession
              (match contents with None -> "" | Some s -> s) ""
            in
            sessions <- SMap.add filename sess sessions;
            sess

      method display_error msg = Ojsmsg_js.display_text_error msg_id msg
      method display_message msg = Ojsmsg_js.display_text_message msg_id msg

      method display_filename ed fname =
        let node = Ojs_js.node_by_id filename_id in
        Ojs_js.clear_children node ;
        let t = Dom_html.document##createTextNode (Js.string (Ojs_path.to_string fname)) in
        Dom.appendChild node t

      method simple_call : 'clt -> unit Lwt.t = fun msg ->
        call msg
          (fun msg -> Lwt.return
             (match msg with
              | P.SError msg -> self#display_error msg
              | P.SOk msg -> self#display_message msg
              | _ -> ()
             )
          )
      method save =
        match current_file with
          None -> Lwt.return_unit
        | Some file ->
            let contents = Js.to_string (editor##getValue()) in
            self#simple_call (P.Save_file (file, contents))

      method edit_file ?contents path =
        let sess = self#get_session ?contents path in
        editor##setSession(sess);
        current_file <- Some path ;
        self#display_filename editor path ;
        let mode =
          let mode =
            Ojs_ace.modeList##getModeForPath(Js.string (Ojs_path.to_string path))
          in
          mode##mode
        in
        (*log("mode to set: "^(Js.to_string mode));*)
        sess##setMode(mode)


      method handle_message (msg : 'srv) =
        try
          (match msg with
           | P.SFile_contents (path, contents) ->
               self#edit_file ~contents path
           | P.SOk msg -> self#display_message msg
           | P.SError msg -> self#display_error msg
           | _ -> failwith "Unhandled message received from server"
          );
          Js._false
        with
          e ->
            log (Printexc.to_string e);
            Js._false

      initializer
        Ojs_js.set_onclick button (fun _ -> self#save);
    end

    class editors
      (call : P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t)
        (send : P.app_client_msg -> unit)
        (spawn : (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit) ->
           bar_id: string -> msg_id: string -> string -> editor) =
        object(self)
          val mutable editors = (SMap.empty : editor SMap.t)

          method get_editor id =
            try SMap.find id editors
            with Not_found -> failwith (Printf.sprintf "Invalid editor id %S" id)

          method get_msg_id id = (self#get_editor id)#msg_id

          method handle_message (msg : P.app_server_msg) =
            match P.unpack_server_msg msg with
            | Some (id, msg) -> (self#get_editor id)#handle_message msg
            | None -> Js._false

          method setup_editor ~bar_id ~msg_id ed_id =
            let send msg = send (P.pack_client_msg ed_id msg) in
            let call msg cb =
              let cb msg =
                match P.unpack_server_msg msg with
                | Some (_, msg) -> cb msg
                | None -> Lwt.return_unit
              in
              call (P.pack_client_msg ed_id msg) cb
            in
            let editor = spawn call send ~bar_id ~msg_id ed_id in
            editors <- SMap.add ed_id editor editors
        end

end
