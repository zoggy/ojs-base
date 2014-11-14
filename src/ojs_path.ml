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

(**  *)

type t = {
  abs : bool ;
  path : string list ;
  } [@@deriving yojson]

let dir_sep = String.get Filename.dir_sep 0

let empty = { abs = false ; path = [] }
let root = { abs = true ; path = [] }

let is_absolute p = p.abs
let path p = p.path

let of_string s =
  let path = Ojs_misc.split_string s [dir_sep] in
  let abs = String.length s > 0 && String.get s 0 = dir_sep in
  { abs ; path }

let to_string p =
  Printf.sprintf "%s%s"
    (if p.abs then Filename.dir_sep else "")
    (String.concat Filename.dir_sep p.path)

let basename p =
  match List.rev p.path with
    [] -> failwith "Ojs_path.basename: Invalid argument"
  | f :: _ -> f

let parent p =
  match List.rev p.path with
  | [] -> { abs = false ; path = [] }
  | _ :: q -> { p with path = List.rev q }

let append p1 l = { p1 with path = p1.path @ l }
let append_path p1 p2 = { p1 with path = p1.path @ p2.path }

let is_prefix =
  let rec iter = function
    h1 :: q1, h2 :: q2 -> h1 = h2 && iter (q1, q2)
  | [], _ -> true
  | _ :: _, [] -> false
  in
  fun p1 p2 -> iter (p1.path, p2.path)

let normalize path =
    let rec iter acc = function
      [] -> List.rev acc
    | h :: q ->
        if h = Filename.current_dir_name then
          iter acc q
        else
          if h = Filename.parent_dir_name then
            match acc with
              [] -> []
            | _ :: r -> iter r q
          else
            iter (h::acc) q
    in
    { path with path = iter [] path.path}
