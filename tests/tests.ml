open! Otr

let router =
  Otr.(
    create
      [ {%otr| /home/about |} >- "about page"
      ; ([%otr "/home/:int/"]
        >- fun i -> "Product Page. Product Id : " ^ string_of_int i)
      ; ([%otr "/home/:float/"]
        >- fun f -> "Float page. number : " ^ string_of_float f)
      ; ([%otr "/contact/*/:int"]
        >- fun name number ->
        "Contact page. Hi, " ^ name ^ ". Number " ^ string_of_int number)
      ; {%otr| /home/products/** |} >- "full splat page"
      ])

let () =
  assert (Some "Float page. number : 100001.1" = match' router "/home/100001.1/");
  assert (None = match' router "/home/100001.1");
  assert (
    Some "Product Page. Product Id : 100001" = match' router "/home/100001/");
  assert (Some "about page" = match' router "/home/about");
  assert (None = match' router "/home/about/");
  assert (
    Some "Contact page. Hi, bikal. Number 123456"
    = match' router "/contact/bikal/123456");
  assert (
    Some "full splat page"
    = match' router "/home/products/asdfasdf\nasdfasdfasd");
  assert (Some "full splat page" = match' router "/home/products/");
  assert (None = match' router "/home/products");
  assert (None = match' router "/home/")
