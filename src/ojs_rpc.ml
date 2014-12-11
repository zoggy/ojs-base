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

type 'a t = {
  mutable pending : 'a Lwt_condition.t Idmap.t ;
  }

let rpc_handler () = { pending = Idmap.empty }

type 'a call_msg = [
  | `Call of call_id * 'a
  ] [@@deriving yojson]

type 'a return_msg = [
  | `Return of call_id * 'a
  ] [@@deriving yojson]

type 'a msg = [ 'a return_msg | 'a call_msg ] [@@deriving yojson]

let call send t msg callback =
  let id = gensym () in
  let cond = Lwt_condition.create () in
  t.pending <- Idmap.add id cond t.pending ;
  let msg = `Call (id, msg) in
  send msg >>=
  fun () -> Lwt_condition.wait cond >>= callback

let return send call_id msg =
  let msg = `Return (call_id, msg) in
  send msg

let on_return t call_id msg =
  match Idmap.find call_id t.pending with
  | exception Not_found -> ()
  | cond ->
     t.pending <- Idmap.remove call_id t.pending ;
     Lwt_condition.signal cond msg

