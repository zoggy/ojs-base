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
