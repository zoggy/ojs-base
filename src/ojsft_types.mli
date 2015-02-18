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

(** All paths should be relative to root directory. *)
type path = Ojs_path.t

type mime_type = string

type file_tree =
    [ `Dir of string * file_tree list | `File of string * mime_type ]

module type B =
  sig
    type server_msg = .. [@@deriving yojson]
    type server_msg +=
        SOk
      | SError of string
      | STree of file_tree list
      | SAdd_file of path * mime_type
      | SAdd_dir of path
      | SDelete of path

    type client_msg = .. [@@deriving yojson]
    type client_msg +=
        Get_tree
      | Add_file of path * string
      | Add_dir of path
      | Delete of path
      | Rename of path * path
  end

module Make_base : functor () -> B

module type P =
  sig
    include Ojs_types.App_msg
    include B

    val pack_server_msg : string -> server_msg -> app_server_msg
    val unpack_server_msg : app_server_msg -> (string * server_msg) option
    val pack_client_msg : string -> client_msg -> app_client_msg
    val unpack_client_msg : app_client_msg -> (string * client_msg) option
  end

module type Default_P =
  sig
    include P
    type app_server_msg += SFiletree of string * server_msg
    type app_client_msg += Filetree of string * client_msg
  end

module Default : functor (App : Ojs_types.App_msg) -> Default_P
  with type app_server_msg = App.app_server_msg
   and type app_client_msg = App.app_client_msg

