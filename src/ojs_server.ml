
let (>>=) = Lwt.bind

module J = Yojson.Safe

let handle_messages msg_of_wsdata wsdata_of_msg
  handle_message stream push =
  let push_msg msg =
    let wsdata = wsdata_of_msg msg in
    let frame = Websocket.Frame.of_string wsdata in
    Lwt.return (push (Some frame))
  in
  let f frame =
    let s = Websocket.Frame.content frame in
    try
      match msg_of_wsdata s with
        None -> Lwt.return_unit
      | Some msg -> handle_message push_msg msg
    with
    | e ->
        Lwt.return (prerr_endline (Printexc.to_string e))
  in
  Lwt.catch
    (fun _ -> Lwt_stream.iter_s f stream)
    (fun _ -> Lwt.return_unit)

