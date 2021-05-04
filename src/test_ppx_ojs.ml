
module F = [%ojs.form "example/example_form.tmpl"]


let () =
  let form = F.template_ ~form_title: "Title" () in
  let page = Ojs_tmpl.html_page ~page_title:"Test page" ~body: form () in
  print_endline (Xtmpl_rewrite.to_string page)
 