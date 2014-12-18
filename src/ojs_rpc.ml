(** Remote calls *)

let (>>=) = Lwt.(>>=)

type call_id = int [@@deriving yojson]
module Idmap = Map.Make
  (struct type t = call_id let compare = Pervasives.compare end)
module J = Yojson.Safe
type json = J.json

let gensym =
  let cpt = ref 0 in
  fun () -> incr cpt; !cpt

module Base (P:Ojs_types.App_msg) = struct
    type app_server_msg = P.app_server_msg = .. [@@deriving yojson]
    type app_server_msg +=
      | SCall of call_id * app_server_msg
      | SReturn of call_id * app_server_msg
      [@@deriving yojson]

    type app_client_msg = P.app_client_msg = .. [@@deriving yojson]
    type app_client_msg +=
      | Call of call_id * app_client_msg
      | Return of call_id * app_client_msg
      [@@deriving yojson]

    let pack_server_call call_id msg = SCall (call_id, msg)
    let pack_server_return call_id msg = SReturn (call_id, msg)

    let pack_client_call call_id msg = Call (call_id, msg)
    let pack_client_return call_id msg = Return (call_id, msg)
  end

module type P =
  sig
    include Ojs_types.App_msg
    val pack_server_call : call_id -> app_server_msg -> app_server_msg
    val pack_server_return : call_id -> app_server_msg -> app_server_msg
    val pack_client_call : call_id -> app_client_msg -> app_client_msg
    val pack_client_return : call_id -> app_client_msg -> app_client_msg
  end

module type Pspec =
  sig
    type app_server_msg = ..
    type app_client_msg = ..
    val pack_call_msg : call_id -> app_server_msg -> app_server_msg
    val pack_return_msg : call_id -> app_server_msg -> app_server_msg
  end

module Make (P:Pspec) = struct
    type app_server_msg = P.app_server_msg = ..
    type app_client_msg = P.app_client_msg = ..
    type t = {
        mutable pending : app_client_msg Lwt_condition.t Idmap.t ;
        send : app_server_msg -> unit Lwt.t;
      }

    let rpc_handler send = { pending = Idmap.empty ; send }

    let call t msg callback =
      let id = gensym () in
      let cond = Lwt_condition.create () in
      t.pending <- Idmap.add id cond t.pending ;
      let msg = P.pack_call_msg id msg in
      t.send msg >>=
        fun () -> Lwt_condition.wait cond >>= callback

    let return t call_id msg =
      let msg = P.pack_return_msg call_id msg in
      t.send msg

    let on_return t call_id msg =
      match Idmap.find call_id t.pending with
      | exception Not_found -> ()
      | cond ->
        begin
          t.pending <- Idmap.remove call_id t.pending ;
          Lwt_condition.signal cond msg
        end
  end
module Make_server (P:P) = struct
   module Pspec = struct
        type app_server_msg = P.app_server_msg = ..
        type app_client_msg = P.app_client_msg = ..
        let pack_call_msg = P.pack_server_call
        let pack_return_msg = P.pack_server_return
      end
    include Make(Pspec)
  end

module Make_client (P:P) = struct
   module Pspec = struct
        type app_server_msg = P.app_client_msg = ..
        type app_client_msg = P.app_server_msg = ..
        let pack_call_msg = P.pack_client_call
        let pack_return_msg = P.pack_client_return
      end
    include Make(Pspec)
  end
