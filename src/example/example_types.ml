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

(** *)


(*
type server_msg =
  [ Ojsft_types.server_msg | Ojsed_types.server_msg ]
    [@@deriving Yojson]

type client_msg =
  [ Ojsft_types.client_msg | Ojsed_types.client_msg ]
    [@@deriving Yojson]
*)

type path = string [@@deriving Yojson]
type server_msg =
  [
    `Filetree_msg of string *
      [
      | `Error of string
      | `Tree of Ojsft_types.file_tree list
      | `Add_file of path
      | `Add_dir of path
      | `Del_file of path
      | `Del_dir of path
      | `Rename of path * path
      ]
  | `Editor_msg of string *
      [
      | `Error of string
      | `Ok of string
      | `File_contents of path * string
      ]
  ] [@@deriving Yojson]

type client_msg = [
    `Filetree_msg of string *
      [
      | `Get_tree
      | `Add_file of path
      | `Add_dir of path
      | `Del_file of path
      | `Del_dir of path
      | `Rename of path * path
      ]
  | `Editor_msg of string *
      [
      | `Get_file_contents of path
      | `Save_file of path * string
      ]
  ] [@@deriving Yojson]
