
(** *)

open Ojs_server


let wsdata_of_msg msg = J.to_string (Ojsed_types.server_msg_to_yojson msg)
let msg_of_wsdata s =
  try
    let json = J.from_string s in
    match Ojsed_types.client_msg_of_yojson json with
      `Error s -> raise (Yojson.Json_error s)
    | `Ok msg -> Some msg
  with
    Yojson.Json_error s ->
      prerr_endline s;
      None
  | e ->
      prerr_endline (Printexc.to_string e);
      None

let send_msg push_msg id msg = push_msg (`Editor_msg (id, msg))

let handle_client_msg root id msg =
  match msg with
    `Get_file_contents path ->
      let file = Filename.concat root path in
      let contents = Ojsed_files.string_of_file file in
      (id, [`File_contents (path, contents)])
  | `Save_file (path, contents) ->
      let file = Filename.concat root path in
      Ojsed_files.file_of_string ~file contents ;
      (id, [`Ok "File saved"])
  | _ ->
      failwith "Unhandled message"

let send_messages push_msg (id, messages) =
  Lwt_list.iter_s (send_msg push_msg id) messages

let handle_message root push_msg msg =
  try
    match msg with
    | `Editor_msg (id, t) ->
        Lwt.catch
          (fun () -> send_messages push_msg (handle_client_msg root id t))
          (fun e ->
             let msg =
               match e with
                 Failure s | Sys_error s -> s
               | _ -> Printexc.to_string e
             in
             send_msg push_msg id (`Error msg)
          )
  with
  | e ->
      Lwt.return (prerr_endline (Printexc.to_string e))

