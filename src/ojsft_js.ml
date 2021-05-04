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

open Js_of_ocaml
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

let tree_nodes = ref (SMap.empty : tree_node SMap.t)

let button_bar_class = Ojs_js.class_"button-bar"
let button_class = Ojs_js.class_"button"
let collapsed_class = "collapsed"

let button_bar base_id =
  let doc = Dom_html.document in
  let id = base_id^"-button-bar" in
  let div = doc##createElement (Js.string "div") in
  div##setAttribute (Js.string "id") (Js.string id);
  div##.className := Js.string button_bar_class ;
  div

let add_button id ?cls text bar =
  let doc = Dom_html.document in
  let span = doc##createElement (Js.string "span") in
  span##setAttribute (Js.string "id") (Js.string id);
  span##.className := Js.string button_class ;
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

let add_button_delete base_id bar =
  let id = base_id^"-delete" in
  let cls = button_class^"-delete" in
  let span = add_button id ~cls "✘" bar in
  span

let drag_class = Ojs_js.class_"drag"

let preventDefault evt = ignore(Js.Unsafe.meth_call evt "preventDefault" [| |])
let stopPropagation evt = ignore(Js.Unsafe.meth_call evt "stopPropagation" [| |])

let expand_buttons ?(start=`Collapsed) base_id subs subs_id =
  let doc = Dom_html.document in
  let id_exp = base_id^"expand" in
  let id_col = base_id^"collapse" in

  let span_exp = doc##createElement (Js.string "span") in
  span_exp##setAttribute (Js.string "id") (Js.string id_exp);

  let span_col = doc##createElement (Js.string "span") in
  span_col##setAttribute (Js.string "id") (Js.string id_col);

  (match start with
   | `Expand ->
       span_exp##.className := Js.string collapsed_class ;
       Ojs_js.node_unset_class subs collapsed_class
   | `Collapsed ->
       span_col##.className := Js.string collapsed_class ;
       Ojs_js.node_set_class subs collapsed_class
  );

  let t_exp = doc##createTextNode (Js.string " ▶") in
  let t_col = doc##createTextNode (Js.string " ▼") in
  Dom.appendChild span_exp t_exp;
  Dom.appendChild span_col t_col;
  Ojs_js.set_onclick span_exp
    (fun e ->
       Ojs_js.set_class ~id: id_exp collapsed_class ;
       Ojs_js.unset_class ~id: id_col collapsed_class ;
       Ojs_js.unset_class ~id: subs_id collapsed_class
    );
  Ojs_js.set_onclick span_col
    (fun e ->
       Ojs_js.set_class ~id: id_col collapsed_class ;
       Ojs_js.unset_class ~id: id_exp collapsed_class ;
       Ojs_js.set_class ~id: subs_id collapsed_class
    );

  (span_exp, span_col)

module type S =
  sig
    module P : Ojsft_types.P
    class tree :
      (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.client_msg -> unit Lwt.t) ->
        msg_id:string ->
        Ojs_js.SMap.key ->
        object ('a)
          val mutable filetree : tree_node list
          val mutable on_deselect : 'a -> Ojsft_types.path -> unit
          val mutable on_select :
            'a ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          val mutable selected : (Ojs_js.id * Ojsft_types.path) option
          val mutable show_files : bool
          method add_dir : Ojsft_types.path -> string -> unit Lwt.t
          method add_file :
            [ `Dir | `File ] -> Ojsft_types.path -> File.file Js.t -> unit
          method build_from_tree : Ojsft_types.file_tree list -> unit
          method compare_tn : tree_node -> tree_node -> int
          method delete : Ojsft_types.path -> unit Lwt.t
          method display_error : string -> unit
          method handle_add_dir : Ojsft_types.path -> unit
          method handle_add_file :
            Ojsft_types.path -> Ojsft_types.mime_type -> unit
          method handle_delete : Ojsft_types.path -> unit
          method handle_drag_drop :
            [ `Dir | `File ] ->
              Ojsft_types.path -> Dom_html.element Js.t -> unit
          method handle_message : P.server_msg -> bool Js.t
          method id : Ojs_js.SMap.key
          method insert_dir : Ojsft_types.path -> unit
          method insert_file :
            Ojsft_types.path -> Ojsft_types.mime_type -> unit
          method insert_tn :
            Ojs_js.id ->
              tree_node -> Dom.node Js.t -> tree_node list -> tree_node list
          method msg_id : string
          method on_deselect : 'a -> Ojsft_types.path -> unit
          method on_select :
            'a ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          method prompt_add_dir : Ojsft_types.path -> unit Lwt.t
          method prompt_delete : Ojsft_types.path -> unit Lwt.t
          method set_on_deselect : ('a -> Ojsft_types.path -> unit) -> unit
          method set_on_select :
            ('a ->
             [ `Dir | `File of Ojsft_types.mime_type ] ->
               Ojsft_types.path -> unit) ->
              unit
          method set_onclick :
            Dom_html.element Js.t ->
              Ojs_js.SMap.key ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          method set_selected :
            Ojs_js.SMap.key ->
              [ `Dir | `File of Ojsft_types.mime_type ] ->
              Ojsft_types.path -> unit
          method set_show_files : bool -> unit
          method set_unselected : Ojs_js.SMap.key -> Ojsft_types.path -> unit
          method simple_call : P.client_msg -> unit Lwt.t
          method tree_node_by_path : Ojsft_types.path -> tree_node
          method update_tree : unit Lwt.t
        end

    class trees :
      (P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t) ->
        (P.app_client_msg -> unit Lwt.t) ->
        ((P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) -> msg_id:string -> string -> tree) ->
        object
          val mutable trees : tree Ojs_js.SMap.t
          method get_msg_id : Ojs_js.SMap.key -> string
          method get_tree : Ojs_js.SMap.key -> tree
          method handle_message : P.app_server_msg -> bool Js.t
          method setup_filetree : msg_id:string -> Ojs_js.SMap.key -> tree
        end
  end

module Make(P:Ojsft_types.P) =
  struct
    module P = P
    class tree call (send : P.client_msg -> unit Lwt.t) ~msg_id id =
      object(self:'self)
        val mutable selected = (None :  (id * Ojs_path.t) option )
        val mutable filetree = ([] : tree_node list)

        val mutable on_select  :'self -> [`Dir | `File of string] -> Ojs_path.t -> unit = fun _ _ _ -> ()
        val mutable on_deselect : 'self -> Ojs_path.t -> unit = fun  _ _ -> ()
        val mutable show_files = true

        method set_on_select f = on_select <- f
        method set_on_deselect f = on_deselect <- f
        method set_show_files b = show_files <- b

        method on_select = on_select
        method on_deselect = on_deselect

        method id : string = id
        method msg_id : string = msg_id

        method display_error msg = Ojsmsg_js.display_text_error msg_id msg

        method simple_call : P.client_msg -> unit Lwt.t = fun msg ->
          call msg
            (fun msg -> Lwt.return
               (match msg with
                | P.SError msg -> self#display_error msg
                | _ -> ()
               )
            )

        method set_unselected div_id path =
          (
           try
             let span_id = (SMap.find div_id !tree_nodes).tn_span_id in
             Ojs_js.unset_class ~id: span_id "selected" ;
           with Not_found -> ()
          );
          selected <- None ;
          self#on_deselect self path

        method set_selected div_id kind path =
          (
           try
             let span_id = (SMap.find div_id !tree_nodes).tn_span_id in
             Ojs_js.set_class ~id: span_id "selected" ;
           with Not_found -> ()
          );
          selected <- Some (div_id, path) ;
          self#on_select self kind path

        method set_onclick (node : Dom_html.element Js.t) div_id kind fname =
          let f _ =
            match selected with
            | None ->  self#set_selected div_id kind fname
            | Some (old_id,l) when id <> div_id ->
                self#set_unselected old_id l ;
                self#set_selected div_id kind fname
            | _ -> ()
          in
          set_onclick node f

        method add_file kind path (file : File.file Js.t) =
          let dir =
            match kind with
              `Dir -> path
            | `File -> Ojs_path.parent path
          in
          let path = Ojs_path.append dir [Js.to_string file##.name] in
          let (size : int) = file##.size in
          let (blob : File.blob Js.t) =
            Js.Unsafe.meth_call file "slice" [| Js.Unsafe.inject 0 ; Js.Unsafe.inject size |]
          in
          let on_success contents =
            let contents = Js.to_string contents in
            let len = String.length contents in
            (* the base64 data is after the first comma, see
               http://css-tricks.com/data-uris/
               *)
            let p = try String.index contents ',' with _ -> failwith "No Base64" in
            ignore(self#simple_call (P.Add_file (path, String.sub contents (p+1) (len - p - 1))))
          in
          let on_error exn =
            log (Printf.sprintf "Reading file: %s" (Printexc.to_string exn))
          in
          (* read in base 64 *)
          let read blob =
            let reader = new%js File.fileReader in
            let res = reader##.result in
            Js.Opt.case (File.CoerceTo.string res)
              (fun () -> Lwt.return (Js.string ""))
              (fun s -> Lwt.return s)
          in
          Lwt.on_any (read blob) on_success on_error

        method add_dir path name =
          let path = Ojs_path.append path [name] in
          self#simple_call (P.Add_dir path)

        method prompt_add_dir path =
          let answer = Dom_html.window##prompt(Js.string "Create directory") (Js.string "") in
          Js.Opt.case answer
            (fun () -> Lwt.return_unit)
            (fun name -> self#add_dir path (Js.to_string name))

        method delete path = self#simple_call (P.Delete path)

        method prompt_delete path =
          let msg = Printf.sprintf "Delete %S ?" (Ojs_path.to_string path) in
          if Js.to_bool (Dom_html.window##confirm(Js.string msg)) then
            self#delete path
          else
            Lwt.return_unit

        method handle_drag_drop kind fname node =
          let on_dragover evt =
            stopPropagation evt;
            preventDefault evt;
            evt##.dataTransfer##.dropEffect := Js.string "copy" ;
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
            Ojs_js.node_unset_class node drag_class ;
            let files = evt##.dataTransfer##.files in
            let len = files##.length in
            for i = 0 to len - 1 do
              Js.Opt.case (files##item(i))
                (fun () -> ())
                (fun file -> self#add_file kind fname file)
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

        method tree_node_by_path path =
          let rec iter trees path =
            match trees, path with
              [], _
            | _, [] -> None
            | tn :: q, [name] when tn.tn_basename = name -> Some tn
            | tn :: q, name :: qpath when tn.tn_basename = name -> iter tn.tn_subs qpath
            | _ :: q, _ -> iter q path
          in
          match iter filetree (Ojs_path.path path) with
            None ->
              (*log (Printf.sprintf "no tree_node for path %s" (Ojs_path.to_string path));*)
              raise Not_found
          | Some tn -> tn

        method compare_tn tn1 tn2 =
          match tn1.tn_type, tn2.tn_type with
            `Dir, `Dir
          | `File, `File -> Stdlib.compare tn1.tn_path tn2.tn_path
          | `Dir, _ -> -1
          | `File, _ -> 1

        method insert_tn parent_id tn node l =
          let parent_node = Ojs_js.node_by_id parent_id in
          let insert pos =
            let children = parent_node##.childNodes in
            let child = children##item pos in
            ignore(parent_node##insertBefore node child)
          in
          let delete pos =
            let children = parent_node##.childNodes in
            let nth_child = children##item pos in
            Js.Opt.case nth_child
              (fun () -> ())
              (fun child -> ignore(parent_node##removeChild child));
          in
          let rec iter pos acc = function
            [] ->
              insert pos ;
              List.rev (tn :: acc)
          | tn2 :: q ->
              match self#compare_tn tn tn2 with
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

        method insert_file path mime =
          let parent = Ojs_path.parent path in
          let basename = Ojs_path.basename path in
          if show_files then
            begin
              match self#tree_node_by_path path with
                tn -> ()
              | exception Not_found ->
      let doc = Dom_html.document in
      let div = doc##createElement (Js.string "div") in
      let div_id = Ojs_js.gen_id () in
      div##setAttribute (Js.string "id") (Js.string div_id);
      div##setAttribute (Js.string "class") (Js.string "ojsft-file");

      let head = doc##createElement (Js.string "div") in
      let head_id = div_id^"-head" in
      head##setAttribute (Js.string "id") (Js.string head_id);
      head##setAttribute (Js.string "class") (Js.string "ojsft-file-head");

      let span_id = div_id^"text" in
      let span = doc##createElement (Js.string span_id) in
      span##setAttribute (Js.string "id") (Js.string (div_id^"text"));
      self#set_onclick span div_id (`File mime) path ;

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
          let tn = self#tree_node_by_path parent in
          match tn.tn_subs_id with
            None -> raise Not_found
          | Some id -> (id, tn.tn_subs, (fun l -> tn.tn_subs <- l))
        with Not_found ->
            (id, filetree, (fun l -> filetree <- l))
      in
      let items = self#insert_tn parent_id tn (div :> Dom.node Js.t) items in
      update_items items ;

      let text = doc##createTextNode (Js.string basename) in

      let bbar = button_bar div_id in
      let btn_delete = add_button_delete div_id bbar in
      Ojs_js.set_onclick btn_delete (fun _ -> self#prompt_delete path);

      Dom.appendChild div head ;
      Dom.appendChild head span ;
      Dom.appendChild span text ;
      Dom.appendChild head bbar ;

      self#handle_drag_drop `File path div ;
            end

        method insert_dir path =
          let parent = Ojs_path.parent path in
          let basename = Ojs_path.basename path in
          match self#tree_node_by_path path with
            tn -> ()
          | exception Not_found ->
      let doc = Dom_html.document in
      let div = doc##createElement (Js.string "div") in
      let div_id = Ojs_js.gen_id () in
      div##setAttribute (Js.string "id") (Js.string div_id);
      div##setAttribute (Js.string "class") (Js.string "ojsft-dir");

      let head = doc##createElement (Js.string "div") in
      let head_id = div_id^"-head" in
      head##setAttribute (Js.string "id") (Js.string head_id);
      head##setAttribute (Js.string "class") (Js.string "ojsft-file-head");

      let span_id = div_id^"text" in
      let span = doc##createElement (Js.string "span") in
      span##setAttribute (Js.string "id") (Js.string span_id);
      self#set_onclick span div_id `Dir path ;

      let subs_id = div_id^"subs" in
      let div_subs = doc##createElement (Js.string "div") in
      div_subs##setAttribute (Js.string "id") (Js.string subs_id);
      div_subs##setAttribute (Js.string "class") (Js.string "ojsft-dir-subs");

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
          let tn = self#tree_node_by_path parent in
          match tn.tn_subs_id with
            None -> raise Not_found
          | Some id -> (id, tn.tn_subs, (fun l -> tn.tn_subs <- l))
        with Not_found ->
            (id, filetree, (fun l -> filetree <- l))
      in
      let items = self#insert_tn parent_id tn (div :> Dom.node Js.t) items in
      update_items items ;

      let (span_exp, span_col) = expand_buttons div_id div_subs subs_id in
      let bbar = button_bar div_id in
      let btn_add_dir = add_button_add_dir div_id bbar in
      let btn_delete = add_button_delete div_id bbar in
      Ojs_js.set_onclick btn_add_dir (fun _ -> self#prompt_add_dir path);
      Ojs_js.set_onclick btn_delete (fun _ -> self#prompt_delete path);

      Dom.appendChild div head ;
      Dom.appendChild head span ;
      Dom.appendChild span text ;
      Dom.appendChild head span_exp ;
      Dom.appendChild head span_col ;
      Dom.appendChild head bbar ;
      Dom.appendChild div div_subs ;

      self#handle_drag_drop `Dir path head

        method build_from_tree (tree_files : Ojsft_types.file_tree list) =
          let node = Ojs_js.node_by_id id in
          Ojs_js.clear_children node ;
          let rec insert path = function
            `Dir (s, l) ->
              let path = Ojs_path.append path [s] in
              self#insert_dir path ;
              List.iter (insert path) l

          | `File (s, mime) ->
              let path = Ojs_path.append path [s] in
              self#insert_file path mime
          in
          List.iter (insert Ojs_path.empty) tree_files

        method handle_add_file path =
          self#insert_file path

        method handle_add_dir path =
          self#insert_dir path

        method handle_delete path =
          match self#tree_node_by_path path with
          | exception Not_found -> log ("handle_delete: path not found: "^(Ojs_path.to_string path))
          | tn ->
              tree_nodes := SMap.remove tn.tn_id !tree_nodes;
              (match Ojs_js.node_by_id tn.tn_id with
               | exception _ -> ()
               | node ->
                 Js.Opt.iter node##.parentNode
                  (fun p -> ignore(p##removeChild((node :> Dom.node Js.t))));
              );
              let filter = List.filter (fun tn2 -> tn2.tn_id <> tn.tn_id) in
              match self#tree_node_by_path (Ojs_path.parent path) with
              | exception Not_found -> filetree <- filter filetree
              | parent_tn -> parent_tn.tn_subs <- filter parent_tn.tn_subs

        method handle_message (msg : 'srv) =
          try
            (match msg with
             | P.STree l -> self#build_from_tree l
             | P.SAdd_file (path, mime) -> self#handle_add_file path mime
             | P.SAdd_dir path -> self#handle_add_dir path
             | P.SError msg -> self#display_error msg
             | P.SDelete path -> self#handle_delete path
             | P.SOk -> ()
             | _ -> failwith "Unhandled message received from server"
            );
            Js._false
          with
            e ->
              log (Printexc.to_string e);
              Js._false

        method update_tree : unit Lwt.t =
          call P.Get_tree
            (function P.STree l -> self#build_from_tree l; Lwt.return_unit | _ -> Lwt.return_unit)

        initializer
          ignore(self#update_tree)

      end

    class trees
      (call : P.app_client_msg -> (P.app_server_msg -> unit Lwt.t) -> unit Lwt.t)
        (send : P.app_client_msg -> unit Lwt.t)
        (spawn : (P.client_msg -> (P.server_msg -> unit Lwt.t) -> unit Lwt.t) ->
         (P.client_msg -> unit Lwt.t) ->
           msg_id: string -> string -> tree) =
        object(self)
          val mutable trees = (SMap.empty : tree SMap.t)

          method get_tree id =
            try SMap.find id trees
            with Not_found -> failwith ("No tree "^id)

          method get_msg_id id = (self#get_tree id)#msg_id

          method setup_filetree ~msg_id id =
            let send msg = send (P.pack_client_msg id msg) in
            let call msg cb =
              let cb msg =
                match P.unpack_server_msg msg with
                | Some (_, msg) -> cb msg
                | None -> Lwt.return_unit
              in
              call (P.pack_client_msg id msg) cb
            in
            let tree = spawn call send ~msg_id id in
            trees <- SMap.add id tree trees;
            tree

          method handle_message (msg : P.app_server_msg) =
            match P.unpack_server_msg msg with
            | Some (id, msg) ->
                let tree = self#get_tree id in
                tree#handle_message msg
            | None -> Js._false

        end
end






