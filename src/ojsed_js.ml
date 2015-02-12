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

type session = {
  sess_ace : Ojs_ace.editSession Js.t ;
  mutable sess_changed : bool ;
}

module PMap = Ojs_path.Map

let mk_button label =
  let doc = Dom_html.document in
  let b = doc##createElement(Js.string "button") in
  let text = doc##createTextNode(Js.string label) in
  Dom.appendChild b text ;
  b

module Make(P:Ojsed_types.P) =
  struct
    class editor call (send : P.client_msg -> unit Lwt.t)
      ~bar_id ~msg_id ed_id =
    let editor = Ojs_ace.ace##edit (Js.string ed_id) in
    let _ = editor##setFontSize(Js.string "14px") in
    let _ = editor##setKeyboardHandler(Js.string "ace/keyboard/emacs") in
    let bar = Ojs_js.node_by_id bar_id in
    let doc = Dom_html.document in
    let btn_save =  mk_button "Save" in
    let btn_reload = mk_button "Reload" in
    let filename_id = ed_id ^ "__filename" in
    let fname = doc##createElement(Js.string "span") in
    let _ =
      fname##setAttribute (Js.string "id", Js.string filename_id) ;
      fname##setAttribute (Js.string "class", Js.string "filename") ;
      Dom.appendChild bar btn_save ;
      Dom.appendChild bar btn_reload ;
      Dom.appendChild bar fname
    in
    object(self)
      val mutable current_file = (None : Ojsed_types.path option)
      val mutable sessions = (PMap.empty : session PMap.t)

      method id = ed_id
      method msg_id = msg_id

      method on_changed path changed =
        match current_file with
        | Some fname when fname = path -> self#display_filename ~changed fname
        | _ -> ()

      method get_session ?contents filename =
        try PMap.find filename sessions
        with Not_found ->
            let sess_ace = Ojs_ace.newEditSession
              (match contents with None -> "" | Some s -> s) ""
            in
            sess_ace##setUndoManager(Ojs_ace.newUndoManager());
            let doc = sess_ace##getDocument() in
            let s = { sess_ace ; sess_changed = false } in
            doc##on(Js.string "change",
             fun _ ->
               if not s.sess_changed then
                 begin s.sess_changed <- true; self#on_changed filename true end
            );
            sessions <- PMap.add filename s sessions;
            s

      method display_error msg = Ojsmsg_js.display_text_error msg_id msg
      method display_message msg = Ojsmsg_js.display_text_message msg_id msg

      method display_filename ?(changed=false) fname =
        let node = Ojs_js.node_by_id filename_id in
        Ojs_js.clear_children node ;
        let fname = Printf.sprintf "%s%s"
          (if changed then "*" else "")
          (Ojs_path.to_string fname)
        in
        let t = Dom_html.document##createTextNode (Js.string fname) in
        Dom.appendChild node t

      method simple_call : ?on_ok: (unit -> unit) -> 'clt -> unit Lwt.t = fun ?on_ok msg ->
        call msg
          (fun msg -> Lwt.return
             (match msg with
              | P.SError msg -> self#display_error msg
              | P.SOk msg ->
                  begin
                    self#display_message msg ;
                    match on_ok with
                    | None -> ()
                    | Some f -> f ()
                  end
              | _ -> ()
             )
          )

      method call_save path contents =
        let on_ok () =
          match PMap.find path sessions with
          | exception Not_found -> ()
          | sess ->
            let b = sess.sess_changed in
            if b then
              begin
                sess.sess_changed <- false ;
                self#on_changed path false
              end
        in
        self#simple_call ~on_ok (P.Save_file (path, contents))

      method save =
        match current_file with
          None -> Lwt.return_unit
        | Some file ->
            let contents = Js.to_string (editor##getValue()) in
            self#call_save file contents

      method save_file path =
        try
          let s = PMap.find path sessions in
          let contents = Js.to_string (s.sess_ace##getValue()) in
          self#call_save path contents
        with Not_found ->
            Lwt.return_unit

      method changed_files =
        PMap.fold (fun path s acc ->
           if s.sess_changed then path :: acc else acc)
           sessions []

      method save_changed_files =
        match self#changed_files with
        | [] -> Lwt.return_unit
        | l -> Lwt_list.iter_p self#save_file l

      method edit_file ?contents path =
        let sess = self#get_session ?contents path in
        editor##setSession(sess.sess_ace);
        current_file <- Some path ;
        self#display_filename path ;
        let mode =
          let mode =
            Ojs_ace.modeList##getModeForPath(Js.string (Ojs_path.to_string path))
          in
          mode##mode
        in
        (*log("mode to set: "^(Js.to_string mode));*)
        sess.sess_ace##setMode(mode)

      method load_from_server path =
        let cb = function
        | P.SFile_contents (path, contents) ->
            begin
              let s = self#get_session path in
              s.sess_ace##setValue (Js.string contents);
              s.sess_changed <- false ;
              self#display_filename ~changed: false path ;
              Lwt.return_unit
            end
        | _ -> Lwt.return_unit
        in
        call (P.Get_file_contents path) cb

      method reload_file path =
        let do_it =
          match PMap.find path sessions with
          | exception Not_found -> true
          | sess ->
            not sess.sess_changed ||
             (
              let msg = Printf.sprintf
               "%s is modified and not saved.\nDo you really want to reload file from server ?"
                (Ojs_path.to_string path)
              in
              Js.to_bool (Dom_html.window##confirm(Js.string msg))
             )
        in
        if do_it then self#load_from_server path else Lwt.return_unit

      method reload =
        match current_file with
        | None -> Lwt.return_unit
        | Some p -> self#reload_file p

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
        Ojs_js.set_onclick btn_save (fun _ -> self#save);
        Ojs_js.set_onclick btn_reload (fun _ -> self#reload);
    end

    class editors
      (call : P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t)
        (send : P.app_client_msg -> unit Lwt.t)
        (spawn : (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) ->
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
            editors <- SMap.add ed_id editor editors;
            editor
        end

end
