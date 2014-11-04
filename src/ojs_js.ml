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

module SMap = Map.Make(String)
let (+=) map (key, v) = map := SMap.add key v !map
let (-=) map key = map := SMap.remove key !map
type id = string

let log s = Firebug.console##log (Js.string s);;

let class_ s = "ojs-"^s

let setup_ws url msg_of_data data_of_msg
  ~onopen ~onmessage =
    let on_message ws _ event =
      try
      log "message received on ws";
      match msg_of_data (Js.to_string event##data) with
        None -> Js._false
      | Some msg ->
          onmessage ws msg;
          Js._false
    with
      e ->
        log (Printexc.to_string e);
        Js._false
    in
    try
      log ("connecting with websocket to "^url);
      let ws = jsnew WebSockets.webSocket(Js.string url) in
      ws##onmessage <- Dom.full_handler (on_message ws) ;
      ws##onclose <- Dom.handler (fun _ -> log "WS now CLOSED"; Js._false);
      ws##onopen <- Dom.handler (fun _ -> onopen ws; Js._false) ;
      Some ws
    with e ->
        log ("Could not connect to "^url);
        log (Printexc.to_string e);
        None
;;

let send_msg ws data = ws##send (Js.string data)

let clear_children node =
  let children = node##childNodes in
  for i = 0 to children##length - 1 do
    Js.Opt.iter (node##firstChild) (fun n -> Dom.removeChild node n)
  done

let node_by_id id =
  let node = Dom_html.document##getElementById (Js.string id) in
  Js.Opt.case node (fun _ -> failwith ("No node with id = "^id)) (fun x -> x)

let gen_id = let n = ref 0 in fun () -> incr n; Printf.sprintf "ojsid%d" !n

let set_onclick node f =
  ignore(Dom_html.addEventListener node
   Dom_html.Event.click
     (Dom.handler (fun e -> f e; Js.bool true))
     (Js.bool true))


(*c==v=[String.split_string]=1.2====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> if keep_empty then [""] else []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.2====*)

let get_classes node =
  let s =Js.to_string node##className in
  split_string s [' ']

let node_unset_class node cl =
  node##classList##remove(Js.string cl)

let node_set_class node cl =
  node##classList##add(Js.string cl)

let unset_class span_id cl =
  try
    let node = node_by_id span_id in
    node_unset_class node cl
  with
    Failure msg -> log msg

let set_class span_id cl =
  try
    let node = node_by_id span_id in
    node_set_class node cl
  with
    Failure msg -> log msg
