
open Ojs_js

type editor_info = {
    ed_id : id ;
    bar_id : id ;
    mbox_id : id ;
    fname_id : id ;
    ws : WebSockets.webSocket Js.t ;
    mutable docs : Ojs_ace.document Js.t SMap.t ;
  }

let editors = ref (SMap.empty : editor_info SMap.t)

let msg_of_wsdata json =
  try
    match Ojsed_types.server_msg_of_yojson (Yojson.Safe.from_string json) with
      `Error s -> failwith (s ^ "\n" ^ json)
    | `Ok msg -> Some msg
  with
    e ->
      log (Printexc.to_string e);
      None

let wsdata_of_msg msg =
  Yojson.to_string (Ojsed_types.client_msg_to_yojson msg)

let send_msg ws id msg =
  let msg = `Editor_msg (id, msg) in
  Ojs_js.send_msg ws (wsdata_of_msg msg)

let get_editor id =
  try SMap.find id !editors
  with Not_found -> failwith (Printf.sprintf "Invalid editor id %S" id)

let display_message ed_id msg =
  let ed = get_editor ed_id in
  let node = Ojs_js.node_by_id ed.mbox_id in
  Ojs_js.clear_children node ;
  let t = Dom_html.document##createTextNode (Js.string msg) in
  Dom.appendChild node t

let save ws ed_id = send_msg ws ed_id (`Save_file ("foo.ml", "let x = 1"))

let handle_message ws msg =
   try
    (match msg with
     | `Editor_msg (id, t) ->
         match t with
           `File_contents (fname, contents) ->
             ()
         | `Ok msg -> display_message id msg
         | _ -> failwith "Unhandled message received from server"
    );
    Js._false
  with
    e ->
      log (Printexc.to_string e);
      Js._false

let build_editor ws bar_id ed_id =
  Ojs_ace.ace##edit (Js.string ed_id);
  let bar = Ojs_js.node_by_id bar_id in
  let doc = Dom_html.document in
  let button = doc##createElement(Js.string "button") in
  let text = doc##createTextNode(Js.string "Save") in

  let fname_id = ed_id ^ "__filename" in
  let fname = doc##createElement(Js.string "span") in
  fname##setAttribute (Js.string "id", Js.string fname_id);
  fname##setAttribute (Js.string "class", Js.string "filename");

  let mbox_id = ed_id ^ "__msg" in
  let mbox = doc##createElement(Js.string "pre") in
  mbox##setAttribute (Js.string "id", Js.string mbox_id);
  mbox##setAttribute (Js.string "class", Js.string "message-box");

  Dom.appendChild bar button ;
  Dom.appendChild button text ;
  Dom.appendChild bar fname ;
  Dom.appendChild bar mbox ;

  Ojs_js.set_onclick button (fun _ -> save ws ed_id);

  let ed = {
      ed_id ; bar_id ; mbox_id ; fname_id ;
      ws ;
      docs = SMap.empty ;
    }
  in
  ed

let setup_editor ws ~bar ~editor =
  let ed = build_editor ws bar editor in
  editors += (editor, ed)

