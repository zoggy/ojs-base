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

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

module SMap = Map.Make(String)

let lid loc s = Location.mkloc (Longident.parse s) loc

(*c==v=[File.string_of_file]=1.1====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = Bytes.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_subbytes buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.1====*)

let error loc msg = raise (Location.Error (Location.error ~loc msg))
let kerror loc = Printf.ksprintf (error loc)

let file_path node exp =
  let loc = exp.pexp_loc in
  let base_path =
    match loc.Location.loc_start.Lexing.pos_fname with
    | "" -> Filename.current_dir_name
    | f -> Filename.dirname f
  in
  match exp.pexp_desc with
  | Pexp_constant (Const_string (file, _)) ->
      begin
        match Filename.is_relative file with
        | true -> Filename.concat base_path file
        | false -> file
      end
  | _ -> error loc (Printf.sprintf "String constant expected in %S extension node" node)

let read_template loc file =
  try
    let str = string_of_file file in
    match Xtmpl.xml_of_string str with
      Xtmpl.E(_,_,xmls) -> xmls
    | _ -> assert false
  with
    Sys_error msg -> error loc (Printf.sprintf "File %S: %s" file msg)

type input_kind =
  | Button | Checkbox | Color | Date | Datetime | Datetime_local
  | Email | File | Hidden | Image | Month | Number | Password
  | Radio | Range | Reset | Search | Submit | Tel | Text | Time
  | Url | Week
  | Textarea
  | Select

let input_kind_of_string loc = function
| "button" -> Button
| "checkbox" -> Checkbox
| "color" -> Color
| "date" -> Date
| "datetime" -> Datetime
| "datetime-local" -> Datetime_local
| "email" -> Email
| "file" -> File
| "hidden" -> Hidden
| "image" -> Image
| "month" -> Month
| "number" -> Number
| "password" -> Password
| "radio" -> Radio
| "range" -> Range
| "reset" -> Reset
| "search" -> Search
| "submit" -> Submit
| "tel" -> Tel
| "text" -> Text
| "time" -> Time
| "url" -> Url
| "week" -> Week
| s -> kerror loc "Invalid input type %S" s

type input = {
  i_name : string ;
  i_kind : input_kind ;
  i_mltype : [ `CData | `Other of (string * string * string) (* typ, to_string, of_string *)] ;
  i_value : Xtmpl.tree list option ;
  i_mandatory : bool ;
}

let att_ s = ("", s)
let att_param = att_"param_"
let att_to_xml = att_"to_xml_"
let att_to_string = att_"to_string"
let att_of_string = att_"of_string"
let att_type = att_"type"
let att_mltype = att_"type_"
let att_optional = att_"optional_"
let att_name = att_"name"
let att_mandatory = att_"mand_"
let att_value = att_"value"

let get_name atts = Xtmpl.get_arg_cdata atts att_name

let string_of_name = function ("", s) -> s | (p,s) -> p ^ ":" ^ s

let to_id = String.map
  (function
   | 'a'..'z' as c -> c
   | '0'..'9' as c -> c
   | 'A'..'Z' as c -> Char.lowercase c
   | _ -> '_')


let parse_ocaml_expression loc str =
  let lexbuf = Lexing.from_string str in
  try Parser.parse_expression Lexer.token_with_comments lexbuf
  with e ->
    error loc
        (Printf.sprintf "Error while parsing the following OCaml expression:\n%s\n%s"
         str (Printexc.to_string e))

let parse_ocaml_type loc str =
  let lexbuf = Lexing.from_string str in
  try Parser.parse_core_type Lexer.token_with_comments lexbuf
  with e ->
    error loc
        (Printf.sprintf "Error while parsing the following OCaml type:\n%s\n%s"
         str (Printexc.to_string e))

(* Check: utiliser de checked quand true, absence de la requete quand faux
   Radio: ignorer si deja vu dans les parametres, doit etre decrit par le premier radio dans le xml *)
let input_of_atts loc i_name ?kind atts subs =
  let i_kind =
    match kind with
    | Some k -> k
    | None ->
      match Xtmpl.get_arg_cdata atts att_type with
        | None -> Text
        | Some s -> input_kind_of_string loc s
  in
  let i_mandatory = Xtmpl.get_arg_cdata atts att_mandatory = Some "true" in
  let i_value =
    match i_kind with
      Textarea -> Some subs
    | _ -> Xtmpl.get_arg atts att_value
  in
  let def_type =
    match i_kind with
    | Button
    | Reset
    | Submit -> `CData

    | Date
    | Datetime
    | Datetime_local
    | Time -> `CData

    | Checkbox -> `Other ("bool", "fun _ -> \"true\"", "fun _ -> true")
    | Email | File | Password | Tel | Text | Search | Url | Hidden -> `CData
    | Color | Image -> `CData
    | Textarea -> `CData
    | Radio -> `CData
    | Select -> `CData

    | Month | Week -> `CData
    | Number | Range -> `Other ("int", "string_of_int", "int_of_string")
  in
  let i_mltype =
    match Xtmpl.get_arg_cdata atts att_mltype with
      None -> def_type
    | Some "cdata" -> `CData
    | Some str ->
        match
          Xtmpl.get_arg_cdata atts att_to_string,
          Xtmpl.get_arg_cdata atts att_of_string
        with
        | None, _ -> kerror loc
            "Input %S: Missing attribute %s"
              i_name (string_of_name att_to_string)
        | _, None -> kerror loc
            "Input %S: Missing attribute %s"
              i_name (string_of_name att_of_string)
        | Some to_s, Some of_s ->
            `Other (str, to_s, of_s)
  in
  { i_name ; i_kind ; i_mltype ;
    i_value ; i_mandatory ;
  }

let clear_atts atts =
  List.fold_right Xtmpl.atts_remove
    [ att_mandatory ; att_value ; att_to_string ; att_of_string ]
    atts

let mk_value_param i =
  let value_atts =
    let type_atts =
      match i.i_mltype with
      | `CData -> [ att_mltype, [Xtmpl.D "cdata"] ]
      | `Other (t,to_s,_) ->
          [ att_mltype, [ Xtmpl.D t ] ;
            att_to_xml, [ Xtmpl.D (Printf.sprintf "fun x__ -> [ Xtmpl.D ((%s) x__) ]" to_s) ] ;
          ]
    in
    Xtmpl.atts_of_list
      (( att_param, [ Xtmpl.D "true" ]) ::
      ( att_optional, [ Xtmpl.D "true"] ) ::
        type_atts)
  in
  Xtmpl.E (("",i.i_name), value_atts, match i.i_value with None -> [] | Some l -> l)

let add_atts_of_input i atts =
  let atts =
    match i.i_kind with
    | Textarea -> atts
    | Checkbox -> Xtmpl.atts_one ~atts att_value [ Xtmpl.D "true" ]
    | _ -> Xtmpl.atts_one ~atts att_value [ mk_value_param i ]
  in
  let atts =
    match i.i_kind with
      Checkbox -> Xtmpl.atts_one ~atts ("", "id") [Xtmpl.D i.i_name]
    | _ -> atts
  in
  atts

let xml_of_input i tag atts subs =
  let atts = add_atts_of_input i (clear_atts atts) in
  let subs =
    match i.i_kind with
    | Textarea -> [ mk_value_param i ]
    | Checkbox ->
        let atts = Xtmpl.atts_of_list
          [ ("","type"), [ Xtmpl.D "text/javascript"] ]
        in
        let v = mk_value_param
          { i with i_mltype = `Other ("bool", "function true -> \"true\" | false -> \"false\"", "") }
        in
        let node =
          Xtmpl.E (("","script"), atts,
           [ Xtmpl.D
            (Printf.sprintf "document.getElementById('%s').checked = " i.i_name) ;
             v ;
             Xtmpl.D ";"
           ])
        in
        [ node ]
    | _ -> []
  in
  Xtmpl.E (tag, atts, subs)

let map_textarea loc tag name atts subs =
  let input = input_of_atts loc name ~kind: Textarea atts subs in
  let xml = xml_of_input input tag atts subs in
  (input, xml)

let map_select loc tag name atts subs =
  let input = input_of_atts loc name ~kind: Select atts subs in
  let xml = xml_of_input input tag atts subs in
  (input, xml)

let map_input loc tag name atts subs =
  let input = input_of_atts loc name atts subs in
  let xml = xml_of_input input tag atts subs in
  (input, xml)
let map_button loc tag name atts subs =
  let (i, xml) = map_input loc tag name atts subs in
  match i.i_kind with
    Reset | Submit | Button -> (i, xml)
  | _ -> kerror loc "Invalid type for button %S" name

let with_name acc tag f loc atts subs =
  match get_name atts with
    None ->  (acc, None, Xtmpl.E (tag, atts, subs))
  | Some name ->
    let (p, xml) = f loc tag name atts subs in
    (acc, Some p, xml)

let map_form_tmpl loc tmpl =
  let rec iter_list acc xmls =
    let (acc, xmls) = List.fold_left
      (fun (acc, acc_xmls) xml ->
         let (acc, xml) = iter acc xml in
         (acc, xml :: acc_xmls)
      )
        (acc, []) xmls
    in
    (acc, List.rev xmls)
  and iter acc xml =
    match xml with
      Xtmpl.D _ -> (acc, xml)
    | Xtmpl.E (("", stag) as tag, atts, subs) ->
        begin
          let (acc, i_opt, xml) =
            match stag with
            | "textarea" -> with_name acc tag map_textarea loc atts subs
            | "select" -> with_name acc tag map_select loc atts subs
            | "input" -> with_name acc tag map_input loc atts subs
            | "button" -> with_name acc tag map_button loc atts subs
            | _ ->
                let (acc, xmls) = iter_list acc subs in
                (acc, None, Xtmpl.E (tag, atts, xmls))
          in
          match i_opt with
            None -> (acc, xml)
          | Some i ->
              try
                (* do not replace radio button inputs *)
                ignore(SMap.find i.i_name acc);
                (acc, xml)
              with Not_found ->
                  (SMap.add i.i_name i acc, xml)
        end
    | Xtmpl.E (tag, atts, subs) ->
        let (acc, xmls) = iter_list acc subs in
        (acc, Xtmpl.E (tag, atts, xmls))
  in
  iter_list SMap.empty tmpl

let mk_template loc tmpl =
  Str.value Nonrecursive
    [ Vb.mk (Pat.var (Location.mkloc "template_" loc))
      (Exp.extension
       (Location.mkloc "xtmpl.string" loc,
        (PStr  [(Str.eval (Exp.constant (Const_string (Xtmpl.string_of_xmls tmpl, None))))]))
      )
    ]

let mk_type loc inputs =
  let field name i acc =
    let id = to_id name in
    let typ =
      let str = match i.i_mltype with
        | `CData -> "string"
        | `Other (typ, _, _) -> typ
      in
      let typ = parse_ocaml_type loc str in
      match i.i_kind with
      | Checkbox -> typ
      | _ ->
          if i.i_mandatory then
            typ
          else
            let lid_option = Location.mkloc (Ldot (Lident "*predef*","option")) loc in
            Typ.constr lid_option [typ]
    in
    (Type.field (Location.mkloc id loc) typ) :: acc
  in
  let fields = SMap.fold field inputs [] in
  let ty = Type.mk ~kind: (Ptype_record fields) (Location.mkloc "t" loc) in
  Str.type_ [ty]

let mk_typ_form loc tmpl =
  let str = Exp.constant (Const_string (Xtmpl.string_of_xmls tmpl, None)) in
  let extension =
    Typ.extension (Location.mkloc "xtmpl.string.type" loc, (PStr  [Str.eval str]))
  in
  let ty = Type.mk ~manifest: extension (Location.mkloc "form" loc) in
  Str.type_ [ty]

let mk_typ_template loc tmpl =
  let str = Exp.constant (Const_string (Xtmpl.string_of_xmls tmpl, None)) in
  let extension =
    Typ.extension (Location.mkloc "xtmpl.string.type" loc, (PStr  [Str.eval str]))
  in
  let ty = Type.mk ~manifest: extension (Location.mkloc "template" loc) in
  Str.type_ [ty]

let mk_exn loc =
  Str.exception_
    (Te.decl
      ~args: [ [%type: template * string list] ]
      (Location.mkloc "Error" loc)
    )

let mk_read_form loc inputs =
  let read_input name i exp =
    let id = to_id name in
    let mand = if i.i_mandatory then [%expr true] else [%expr false] in
    let of_string =
      match i.i_kind with
        Checkbox -> [%expr fun _ -> Some true]
      | _ ->
          match i.i_mltype with
            `CData -> [%expr fun v -> Some v]
          | `Other (_,_,of_s) -> [%expr Some (([%e parse_ocaml_expression loc of_s]) v)]
    in
    let e_name = Exp.constant (Const_string (name, None)) in
    [%expr
      let [%p (Pat.var (Location.mkloc id loc))] =
        read_param__ [%e mand] [%e e_name] [%e of_string]
      in
      [%e exp]
    ]
  in
  let body exp =
    [% expr fun get_arg ->
        let errors = ref [] in
        let defs = ref [] in
        let read_param__ mandatory name of_string =
          let v = get_arg name in
          defs := (("", name), fun x _ _ _ -> (x, [Xtmpl.D (match v with None -> "" | Some s -> s)])) :: !defs ;
          try
            match mandatory, v with
            | true, None -> failwith (name^" is mandatory")
            | false, None -> None
            | _, Some v -> of_string v
          with
            e ->
              let msg = match e with
                | Sys_error s | Invalid_argument s | Failure s -> s
                | e -> Printexc.to_string e
              in
              errors := msg :: !errors ;
              None
        in
        [%e exp]
    ]
  in
  let fill_t =
    let field name i acc =
      let lid_name = lid loc (to_id name) in
      let e =
        let id = Exp.ident lid_name  in
        match i.i_kind with
        | Checkbox -> [%expr match [%e id] with None -> false | Some v -> v]
        | _ ->
            match i.i_mandatory with
            | true -> [%expr match [%e id] with None -> assert false | Some v -> v]
            | false -> id
      in
      (lid_name, e) :: acc
    in
    let fields = SMap.fold field inputs [] in
    Exp.record fields None
  in
  let call_form =
    let f name i acc =
      let label = "?"^(to_id name) in
      let exp = Exp.ident (lid loc (to_id name)) in
      (label, exp) :: acc
    in
    let args = SMap.fold f inputs [] in
    Exp.apply [%expr form ~env] args
  in
  let ending =
    [%expr
      match !errors with
        [] -> [%e fill_t]
      | _ ->
          let (f : template) = fun ?env ->
            let env = Xtmpl.env_of_list ?env !defs in
            [%e call_form]
          in
          raise (Error (f, !errors))
    ]
  in
  let reads = SMap.fold read_input inputs ending in
  [%stri let read_form = [%e body reads]]

let map_ojs_form exp =
  let loc = exp.pexp_loc in
  let file = file_path "ojs.form" exp in
  let tmpl = read_template loc file in
  let (inputs, tmpl_form) = map_form_tmpl loc tmpl in
  let typ_form = mk_typ_form loc tmpl_form in
  let typ_template = mk_typ_template loc tmpl in
  let exn = mk_exn loc in
  let val_template = mk_template loc tmpl_form in
  let val_form = [%stri let form = template_ ] in
  let typ = mk_type loc inputs in
  let read_form = mk_read_form loc inputs in
  let items = [typ_form ; typ_template ; exn ; typ ; val_template ; val_form ; read_form] in
  Mod.structure items

let ojs_mapper argv =
  { default_mapper with
    module_expr = fun mapper expr ->
      match expr with
      | { pmod_desc = Pmod_extension ({ txt = "ojs.form"; loc }, pstr)} ->
          begin
            match pstr with
            | PStr [{ pstr_desc = Pstr_eval (exp, _) }] ->
                (* we expect an expression *)
                map_ojs_form exp
            | _ ->
                error loc "[%ojs.form] accepts a string"
          end
      | x -> default_mapper.module_expr mapper x;
  }

let () = register "ojs" ojs_mapper