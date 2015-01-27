
module F = [%ojs.form "example/example_form.tmpl"]

let () = print_endline
  (Xtmpl.string_of_xmls (F.template_ ~form_title: "Title" ())
  )
 