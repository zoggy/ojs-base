
open Ojs_js

type editor_info = {
    root_id : id ;
    ws : WebSockets.webSocket Js.t ;
  }

let editors = ref (SMap.empty : editor_info SMap.t)

let setup_editor id ws =
  let cfg = {
      root_id = id ; ws ;
    }
  in
  Ojs_ace.ace##edit (Js.string id);
  editors += (id, cfg)

