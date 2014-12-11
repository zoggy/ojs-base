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

type file_tree = [
 | `Dir of string * file_tree list
 | `File of string
 ] [@@deriving yojson]

type server_msg =
  [
  | `Error of string
  | `Tree of file_tree list
  | `Add_file of path
  | `Add_dir of path
  | `Del_file of path
  | `Del_dir of path
  | `Rename of path * path
  ]  [@@deriving yojson]


type client_msg = [
  | `Get_tree
  | `Add_file of path * string (* path * (contents in base 64) *)
  | `Add_dir of path
  | `Del_file of path
  | `Del_dir of path
  | `Rename of path * path
  ] [@@deriving yojson]

type 'a msg = [
    `Filetree_msg of string * 'a
  ]
  [@@deriving yojson]
