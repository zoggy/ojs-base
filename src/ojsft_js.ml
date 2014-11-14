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

let (>>=) = Lwt.(>>=)

let log = Ojs_js.log

type node_type = [`File | `Dir ]

type tree_node = {
    tn_id : id ;
    mutable tn_basename : string ;
    mutable tn_path : Ojs_path.t ;
    tn_span_id : id ;
    tn_subs_id : id option ;
    tn_type : node_type ;
    mutable tn_subs : tree_node list ;
  }

type tree_info = {
    root_id : id ;
    msg_id : id ;
    ws : WebSockets.webSocket Js.t ;
    show_files : bool ;
    on_select : tree_info -> Ojs_path.t -> unit ;
    on_deselect : tree_info -> Ojs_path.t -> unit ;
    mutable selected : (id * Ojs_path.t) option ;
    mutable filetree : tree_node list ;
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
  Yojson.Safe.to_string (Ojsft_types.client_msg_to_yojson msg)

let send_msg ws id msg =
  let msg = `Filetree_msg (id, msg) in
  Ojs_js.send_msg ws (wsdata_of_msg msg)

let set_unselected ti div_id path =
  (
   try
     let span_id = (SMap.find div_id !tree_nodes).tn_span_id in
     Ojs_js.unset_class span_id "selected" ;
   with Not_found -> ()
  );
  ti.selected <- None ;
  ti.on_deselect ti path

let set_selected ti div_id path =
  (
   try
     let span_id = (SMap.find div_id !tree_nodes).tn_span_id in
     Ojs_js.set_class span_id "selected" ;
   with Not_found -> ()
  );
  ti.selected <- Some (div_id, path) ;
  ti.on_select ti path

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


let button_bar_class = Ojs_js.class_"button-bar"
let button_class = Ojs_js.class_"button"

let button_bar base_id =
  let doc = Dom_html.document in
  let id = base_id^"-button-bar" in
  let div = doc##createElement (Js.string "div") in
  div##setAttribute (Js.string "id", Js.string id);
  div##className <- Js.string button_bar_class ;
  div

let add_button id ?cls text bar =
  let doc = Dom_html.document in
  let span = doc##createElement (Js.string "span") in
  span##setAttribute (Js.string "id", Js.string id);
  span##className <- Js.string button_class ;
  (match cls with
    None -> ()
  | Some c -> Ojs_js.node_set_class span c
  );

  let t = doc##createTextNode (Js.string text) in
  Dom.appendChild span t ;
  Dom.appendChild bar span ;
  span

let add_button_add_dir base_id bar =
  let id = base_id^"-add-dir" in
  let cls = button_class^"-add-dir" in
  let span = add_button id ~cls "+dir" bar in
  span

let drag_class = Ojs_js.class_"drag"

let preventDefault evt = ignore(Js.Unsafe.meth_call evt "preventDefault" [| |])
let stopPropagation evt = ignore(Js.Unsafe.meth_call evt "stopPropagation" [| |])

let add_file tree_id kind path (file : File.file Js.t) =
  let dir =
    match kind with
      `Dir -> path
    | `File -> Ojs_path.parent path
  in
  let path = Ojs_path.append dir [Js.to_string file##name] in
  let cfg =
    try SMap.find tree_id !trees
    with Not_found -> failwith ("No config for file_tree "^tree_id)
  in
  let (size : int) = file##size in
  let (blob : File.blob Js.t) =
    Js.Unsafe.meth_call file "slice" [| Js.Unsafe.inject 0 ; Js.Unsafe.inject size |]
  in
  let on_success contents =
    send_msg cfg.ws tree_id (`Add_file (path, Js.to_string contents))
  in
  let on_error _ = () in
  Lwt.on_any (File.readAsBinaryString blob) on_success on_error

let handle_drag_drop tree_id kind fname node =
  let on_dragover evt =
    stopPropagation evt;
    preventDefault evt;
    evt##dataTransfer##dropEffect <- Js.string "copy" ;
    Ojs_js.node_set_class node drag_class ;
    Js.bool true
  in
  let on_dragleave evt =
    Ojs_js.node_unset_class node drag_class ;
    Js.bool true
  in
  let on_drop evt =
    stopPropagation evt;
    preventDefault evt;
    let files = evt##dataTransfer##files in
    let len = files##length in
    for i = 0 to len - 1 do
      Js.Opt.case (files##item(i))
        (fun () -> ())
        (fun file -> add_file tree_id kind fname file)
    done;
    Js.bool true
  in
  ignore(Dom_html.addEventListener node
   Dom_html.Event.dragover
     (Dom.handler on_dragover)
     (Js.bool true)
  ) ;
  ignore(Dom_html.addEventListener node
   Dom_html.Event.dragleave
     (Dom.handler on_dragleave)
     (Js.bool true)
  );
  ignore(Dom_html.addEventListener node
   Dom_html.Event.drop
     (Dom.handler on_drop)
     (Js.bool true)
  )

let tree_node_by_path cfg path =
  let rec iter trees path =
    match trees, path with
      [], _
    | _, [] -> None
    | tn :: q, [name] when tn.tn_basename = name -> Some tn
    | tn :: q, name :: qpath when tn.tn_basename = name -> iter tn.tn_subs qpath
    | _ :: q, _ -> iter q path
  in
  match iter cfg.filetree (Ojs_path.path path) with
    None ->
      (*log (Printf.sprintf "no tree_node for path %s" (Ojs_path.to_string path));*)
      raise Not_found
  | Some tn -> tn

let compare_tn tn1 tn2 =
  match tn1.tn_type, tn2.tn_type with
    `Dir, `Dir
  | `File, `File -> Pervasives.compare tn1.tn_path tn2.tn_path
  | `Dir, _ -> -1
  | `File, _ -> 1

let insert_tn parent_id tn node l =
  let parent_node = Ojs_js.node_by_id parent_id in
  let insert pos =
    let children = parent_node##childNodes in
    let child = children##item(pos) in
    ignore(parent_node##insertBefore(node, child))
  in
  let delete pos =
    let children = parent_node##childNodes in
    let nth_child = children##item(pos) in
    Js.Opt.case nth_child
      (fun () -> ())
      (fun child -> ignore(parent_node##removeChild(child)));
  in
  let rec iter pos acc = function
    [] ->
      insert pos ;
      List.rev (tn :: acc)
  | tn2 :: q ->
     match compare_tn tn tn2 with
       0 ->
          (* replace old tn2 by new tn *)
          delete pos;
          insert pos;
          (List.rev (tn :: acc)) @ q
      | n when n > 0 ->
            iter (pos + 1) (tn2 :: acc) q
      | _ ->
          insert pos ;
          (List.rev (tn :: tn2 :: acc)) @ q
  in
  iter 0 [] l

let insert_file ~id cfg path =
  let parent = Ojs_path.parent path in
  let basename = Ojs_path.basename path in
  if cfg.show_files then
    begin
      match tree_node_by_path cfg path with
        tn -> ()
      | exception Not_found ->
          let doc = Dom_html.document in
          let div = doc##createElement (Js.string "div") in
          let div_id = Ojs_js.gen_id () in
          div##setAttribute (Js.string "id", Js.string div_id);
          div##setAttribute (Js.string "class", Js.string "ojsft-file");

          let span_id = div_id^"text" in
          let span = doc##createElement (Js.string span_id) in
          span##setAttribute (Js.string "id", Js.string (div_id^"text"));
          set_tree_onclick id span div_id path;

          let tn = {
              tn_id = div_id ;
              tn_span_id = span_id ;
              tn_basename = basename ;
              tn_path = path ;
              tn_type = `File ;
              tn_subs = [] ;
              tn_subs_id = None ;
            }
          in
          tree_nodes += (div_id, tn) ;

          let (parent_id, items, update_items) =
            try
              let tn = tree_node_by_path cfg parent in
              match tn.tn_subs_id with
                None -> raise Not_found
              | Some id -> (id, tn.tn_subs, (fun l -> tn.tn_subs <- l))
            with Not_found ->
              (id, cfg.filetree, (fun l -> cfg.filetree <- l))
          in
          let items = insert_tn parent_id tn (div :> Dom.node Js.t) items in
          update_items items ;

          let text = doc##createTextNode (Js.string basename) in
          Dom.appendChild div span ;
          Dom.appendChild span text ;

          handle_drag_drop id `File path div ;
    end

let insert_dir ~id cfg path =
  let parent = Ojs_path.parent path in
  let basename = Ojs_path.basename path in
  match tree_node_by_path cfg path with
    tn -> ()
  | exception Not_found ->
      let doc = Dom_html.document in
      let div = doc##createElement (Js.string "div") in
      let div_id = Ojs_js.gen_id () in
      div##setAttribute (Js.string "id", Js.string div_id);
      div##setAttribute (Js.string "class", Js.string "ojsft-dir");

      let head = doc##createElement (Js.string "div") in
      let head_id = div_id^"-head" in
      head##setAttribute (Js.string "id", Js.string head_id);
      head##setAttribute (Js.string "class", Js.string "ojsft-dir-head");

      let span_id = div_id^"text" in
      let span = doc##createElement (Js.string "span") in
      span##setAttribute (Js.string "id", Js.string span_id);
      set_tree_onclick id span div_id path ;

      let subs_id = div_id^"subs" in
      let div_subs = doc##createElement (Js.string "div") in
      div_subs##setAttribute (Js.string "id", Js.string subs_id);
      div_subs##setAttribute (Js.string "class", Js.string "ojsft-dir-subs");

      let text = doc##createTextNode (Js.string basename) in

      let tn = {
          tn_id = div_id ;
          tn_span_id = span_id ;
          tn_basename = basename ;
          tn_path = path ;
          tn_type = `Dir ;
          tn_subs = [] ;
          tn_subs_id = Some subs_id ;
        }
      in
      tree_nodes += (div_id, tn) ;

      let (parent_id, items, update_items) =
        try
          let tn = tree_node_by_path cfg parent in
          match tn.tn_subs_id with
            None -> raise Not_found
          | Some id -> (id, tn.tn_subs, (fun l -> tn.tn_subs <- l))
        with Not_found ->
            (id, cfg.filetree, (fun l -> cfg.filetree <- l))
      in
      let items = insert_tn parent_id tn (div :> Dom.node Js.t) items in
      update_items items ;

      let (span_exp, span_col) = expand_buttons div_id subs_id in
      let bbar = button_bar div_id in
      let _ = add_button_add_dir div_id bbar in

      Dom.appendChild div head ;
      Dom.appendChild head span ;
      Dom.appendChild span text ;
      Dom.appendChild head span_exp ;
      Dom.appendChild head span_col ;
      Dom.appendChild head bbar ;
      Dom.appendChild div div_subs ;

      handle_drag_drop id `Dir path head

let get_tree_info id =
  try SMap.find id !trees
  with Not_found -> failwith ("No config for file_tree "^id)

let build_from_tree ~id tree_files =
  let node = Ojs_js.node_by_id id in
  Ojs_js.clear_children node ;
  let cfg = get_tree_info id in
  let rec insert path = function
    `Dir (s, l) ->
      let path = Ojs_path.append path [s] in
      insert_dir ~id cfg path ;
      List.iter (insert path) l

  | `File s ->
      let path = Ojs_path.append path [s] in
      insert_file ~id cfg path
  in
  List.iter (insert Ojs_path.empty) tree_files

let handle_add_file id path =
  let cfg = get_tree_info id in
  insert_file ~id cfg path

let handle_message ws msg =
   try
    (match msg with
     | `Filetree_msg (id, t) ->
         match t with
           `Tree l -> build_from_tree id l
         | `Add_file path -> handle_add_file id path
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
  ?(on_deselect=fun _ _-> ())
  ~msg_id id =
  let cfg = {
      root_id = id ; msg_id ; ws ;
      on_select ; on_deselect ; show_files ;
      selected = None ;
      filetree = [] ;
    }
  in
  trees += (id, cfg) ;
  send_msg ws id (`Get_tree)


