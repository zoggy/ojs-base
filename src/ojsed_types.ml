
(** All paths should relative to root directory. *)
type path = string [@@deriving Yojson]

type server_msg = [
    `Editor_msg of string *
      [
      | `Error of string
      | `Ok of string
      | `File_contents of path * string
      ]
  ]
  [@@deriving Yojson]

type client_msg = [
    `Editor_msg of string *
      [
      | `Get_file_contents of path
      | `Save_file of path * string
      ]
  ]
  [@@deriving Yojson]