
open Ojs_js

type editor_info = {
    ed_id : id ;
    bar_id : id ;
    ws : WebSockets.webSocket Js.t ;
  }

let editors = ref (SMap.empty : editor_info SMap.t)

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

let save ws ed_id = ()

let build_editor ws bar_id ed_id =
  Ojs_ace.ace##edit (Js.string ed_id);
  let bar = Ojs_js.node_by_id bar_id in
  let doc = Dom_html.document in
  let button = doc##createElement(Js.string "button") in
  let text = doc##createTextNode(Js.string "Save") in
  Dom.appendChild bar button ;
  Dom.appendChild button text ;

  Ojs_js.set_onclick button (fun _ -> save ws ed_id)

let setup_editor ws ~bar ~editor =
  let cfg = {
      ed_id = editor ; bar_id = bar ; ws ;
    }
  in
  build_editor ws bar editor ;
  editors += (editor, cfg)

