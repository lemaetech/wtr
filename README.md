# Otr - OCaml Typed Router 
*(Under development)*

A typed router for OCaml web applications.
```ocaml
let router =
  Otr.create
    [ {%path| /home/about |} >- "about page"
    ; {%path| /home/:int/ |} >- fun i -> "Product Page. Product Id : " ^ string_of_int i)
    ; {%path| /contact/*/:int |} >- fun name number -> "Contact page. Hi, " s ^ ". Number " ^ string_of_int i)
    ; {%path| /home/:float/ |} >- fun f -> "Float page. number : " ^ string_of_float f)
    ]

let _m = Otr.match' router "/home/100001.1"
let _m1 = Otr.match' router "/home/100001"
let _m2 = Otr.match' router "/home/about"
```
