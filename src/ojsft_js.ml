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
open Ojsft_types

let log = Ojs_js.log

type tree_info = {
    root_id : id ;
    ws : WebSockets.webSocket Js.t ;
    show_files : bool ;
    on_select : tree_info -> string -> unit ;
    on_deselect : tree_info -> string -> unit ;
    mutable selected : (id * string) option ;
  }

type node_type = [`File | `Dir of id ] (* `Dir of (div id) *)
type tree_node =
  { tn_content : string ;
    tn_span_id : id ;
    tn_type : node_type ;
  }
let tree_nodes = ref (SMap.empty : tree_node SMap.t)
let trees = ref (SMap.empty : tree_info SMap.t)

let msg_of_wsdata json =
  try
    match Ojsft_types.server_msg_of_yojson (Yojson.Safe.from_string json) with
      `Error s -> failwith (s ^ "\n" ^ json)
    | `Ok msg -> Some msg
  with
    e ->
      log (Printexc.to_string e);
      None

let wsdata_of_msg msg =
  Yojson.to_string (Ojsft_types.client_msg_to_yojson msg)

let send_msg ws id msg =
  let msg = `Filetree_msg (id, msg) in
  Ojs_js.send_msg ws (wsdata_of_msg msg)

let set_unselected ti div_id fname =
  (
   try
     let span_id = (SMap.find div_id !tree_nodes).tn_span_id in
     Ojs_js.unset_class span_id "selected" ;
   with Not_found -> ()
  );
  ti.selected <- None ;
  ti.on_deselect ti fname

let set_selected ti div_id fname =
  (
   try
     let span_id = (SMap.find div_id !tree_nodes).tn_span_id in
     Ojs_js.set_class span_id "selected" ;
   with Not_found -> ()
  );
  ti.selected <- Some (div_id, fname) ;
  ti.on_select ti fname

let set_tree_onclick id node div_id fname =
  let f _ =
    try
      let ti = SMap.find id !trees in
      match ti.selected with
      | None ->  set_selected ti div_id fname
      | Some (old_id,l) when id <> div_id ->
          set_unselected ti old_id l ;
          set_selected ti div_id fname
      | _ -> ()
    with
      Not_found -> ()
  in
  set_onclick node f

let collapsed_class = "collapsed"
let expand_buttons base_id subs_id =
  let doc = Dom_html.document in
  let id_exp = base_id^"expand" in
  let id_col = base_id^"collapse" in

  let span_exp = doc##createElement (Js.string "span") in
  span_exp##setAttribute (Js.string "id", Js.string id_exp);
  span_exp##className <- Js.string collapsed_class ;

  let span_col = doc##createElement (Js.string "span") in
  span_col##setAttribute (Js.string "id", Js.string id_col);
  let t_exp = doc##createTextNode (Js.string " ▶") in
  let t_col = doc##createTextNode (Js.string " ▼") in
  Dom.appendChild span_exp t_exp;
  Dom.appendChild span_col t_col;
  Ojs_js.set_onclick span_exp
    (fun e ->
       Ojs_js.set_class id_exp collapsed_class ;
       Ojs_js.unset_class id_col collapsed_class ;
       Ojs_js.unset_class subs_id collapsed_class
    );
  Ojs_js.set_onclick span_col
    (fun e ->
       Ojs_js.set_class id_col collapsed_class ;
       Ojs_js.unset_class id_exp collapsed_class ;
       Ojs_js.set_class subs_id collapsed_class
    );

  (span_exp, span_col)

let build_from_tree ~id tree_files =
  let doc = Dom_html.document in
  let node = Ojs_js.node_by_id id in
  Ojs_js.clear_children node ;
  let cfg =
    try SMap.find id !trees
    with Not_found -> failwith ("No config for file_tree "^id)
  in
  let rec insert t = function
    `Dir (s, l) ->
      let label = Filename.basename s in
      let div = doc##createElement (Js.string "div") in
      let div_id = Ojs_js.gen_id () in
      div##setAttribute (Js.string "id", Js.string div_id);
      div##setAttribute (Js.string "class", Js.string "ojsft-dir");

      let span_id = div_id^"text" in
      let span = doc##createElement (Js.string "span") in
      span##setAttribute (Js.string "id", Js.string span_id);
      set_tree_onclick id span div_id s ;

      let subs_id = div_id^"subs" in
      let div_subs = doc##createElement (Js.string "div") in
      div_subs##setAttribute (Js.string "id", Js.string subs_id);
      div_subs##setAttribute (Js.string "class", Js.string "ojsft-dir-subs");

      let text = doc##createTextNode (Js.string label) in

      let tn = {
          tn_span_id = span_id ;
          tn_content = label ;
          tn_type = `Dir subs_id ;
          }
      in
      tree_nodes += (div_id, tn) ;

      let (span_exp, span_col) = expand_buttons div_id subs_id in

      Dom.appendChild t div ;
      Dom.appendChild div span ;
      Dom.appendChild span text ;
      Dom.appendChild div span_exp ;
      Dom.appendChild div span_col ;
      Dom.appendChild div div_subs ;
      List.iter (insert div_subs) l

  | `File s ->
      if cfg.show_files then
        begin
          let label = Filename.basename s in
          let div = doc##createElement (Js.string "div") in
          let div_id = Ojs_js.gen_id () in
          div##setAttribute (Js.string "id", Js.string div_id);
          div##setAttribute (Js.string "class", Js.string "ojsft-file");

          let span_id = div_id^"text" in
          let span = doc##createElement (Js.string span_id) in
          span##setAttribute (Js.string "id", Js.string (div_id^"text"));
          set_tree_onclick id span div_id s;

          let tn = {
              tn_span_id = span_id ;
              tn_content = label ;
              tn_type = `File ;
            }
          in
          tree_nodes += (div_id, tn) ;

          let text = doc##createTextNode (Js.string label) in
          Dom.appendChild t div ;
          Dom.appendChild div span ;
          Dom.appendChild span text
        end
  in
  List.iter (insert node) tree_files


let handle_message ws msg =
   try
    (match msg with
     | `Filetree_msg (id, t) ->
         match t with
           `Tree l -> build_from_tree id l
         | _ -> failwith "Unhandled message received from server"
    );
    Js._false
  with
    e ->
      log (Printexc.to_string e);
      Js._false


let setup_filetree
  ws
  ?(show_files=true)
  ?(on_select=fun _ _ -> ())
  ?(on_deselect=fun _ _-> ()) id =
  let cfg = {
      root_id = id ; ws ;
      on_select ; on_deselect ; show_files ;
      selected = None ;
    }
  in
  trees += (id, cfg) ;
  send_msg ws id (`Get_tree)


