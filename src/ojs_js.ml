
let log s = Firebug.console##log (Js.string s);;

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