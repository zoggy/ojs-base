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

(** Utilities. *)

(*i==v=[String.split_string]=1.2====*)
(** Separate the given string according to the given list of characters.
@author Maxence Guesdon
@version 1.2
@param keep_empty is [false] by default. If set to [true],
   the empty strings between separators are kept.
@cgname String.split_string*)
val split_string : ?keep_empty:bool -> string -> char list -> string list
(*/i==v=[String.split_string]=1.2====*)

(*i==v=[String.is_prefix]=1.0====*)
(** [is_prefix pattern s] returns true if string [s] begins with [pattern].
@author Maxence Guesdon
@version 1.0
@cgname String.is_prefix*)
val is_prefix : string -> string -> bool
(*/i==v=[String.is_prefix]=1.0====*)

(*i==v=[File.file_of_string]=1.1====*)
(** [file_of_string ~file str] creates a file named
   [filename] whose content is [str].
@author Fabrice Lefessant
@version 1.1
@raise Sys_error if the file could not be opened.
@cgname File.file_of_string*)
val file_of_string : file:string -> string -> unit
(*/i==v=[File.file_of_string]=1.1====*)

(** [filename_extension fname] returns extension of [fname] (i.e. the string after
  the last dot), or an empty string if there is no dot. *)
val filename_extension : string -> string
