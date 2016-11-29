module Html = Dom_html
let document = Html.document
let jstr = Js.string


let get_context canvas = canvas##(getContext (Dom_html._2d_)) in
  context##strokeStyle <- Js.string "#FF0000";
