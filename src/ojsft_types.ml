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

(** Types of filetree edition. *)

(** All paths should relative to root directory. *)
type path = Ojs_path.t [@@deriving yojson]

type mime_type = string [@@deriving yojson]

type file_tree = [
  | `Dir of string * file_tree list
  | `File of string * mime_type
  ] [@@deriving yojson]

module Base =
  struct

    type server_msg = .. [@@deriving yojson]
    type server_msg +=
      | SOk
      | SError of string
      | STree of file_tree list
      | SAdd_file of path * mime_type
      | SAdd_dir of path
      | SDelete of path
      [@@deriving yojson]

    type client_msg = .. [@@deriving yojson]
    type client_msg +=
      | Get_tree
      | Add_file of path * string (* path * (contents in base 64) *)
      | Add_dir of path
      | Delete of path
      | Rename of path * path
      [@@deriving yojson]
  end
module Make_base() = struct include Base end

module type P =
  sig
    include Ojs_types.App_msg
    include (module type of Base)

    val pack_server_msg : string -> server_msg -> app_server_msg
    val unpack_server_msg : app_server_msg -> (string * server_msg) option

    val pack_client_msg : string -> client_msg -> app_client_msg
    val unpack_client_msg : app_client_msg -> (string * client_msg) option
  end

module Default_P(App:Ojs_types.App_msg) =
  struct
    type app_server_msg = App.app_server_msg = .. [@@deriving yojson]
    type app_client_msg = App.app_client_msg = .. [@@deriving yojson]

    include (Make_base())

    type app_server_msg += SFiletree of string * server_msg [@@deriving yojson]
    type app_client_msg += Filetree of string * client_msg [@@deriving yojson]

    let pack_server_msg id msg = SFiletree (id, msg)
    let unpack_server_msg = function SFiletree (id,msg) -> Some (id,msg) | _ -> None

    let pack_client_msg id msg = Filetree (id, msg)
    let unpack_client_msg = function Filetree (id,msg) -> Some (id,msg) | _ -> None
  end
  