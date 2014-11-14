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

(** Handling file paths. *)

type t [@@deriving yojson]

val dir_sep : char
val empty : t
val root : t

val is_absolute : t -> bool
val path: t -> string list

val of_string : string -> t
val to_string : t -> string

(** @raise Invalid_argument is the path is empty. *)
val basename : t -> string

val parent : t -> t
val append : t -> string list -> t
val append_path : t -> t -> t
val is_prefix : t -> t -> bool

(** [normalize path] returns [path] where [Filename.parent_dir_name]
  and [Filename.current_dir_name] have been handled so they don't appear anymore.
  Note that [/..] becomes [/] (i.e. no error in case there are too many separators).
*)
val normalize : t -> t
