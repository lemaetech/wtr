# Otr - OCaml Typed Router 
*(Unreleased)*

A typed router for OCaml web applications.

```ocaml

open! Otr

let prod_page i = "Product Page. Product Id : " ^ string_of_int i

let float_page f = "Float page. number : " ^ string_of_float f

let contact_page name number =
  "Contact page. Hi, " ^ name ^ ". Number " ^ string_of_int number

let router =
  create
    [ {%otr| /home/about     |} >- "about page"
    ; {%otr| /home/:int/     |} >- prod_page
    ; {%otr| /home/:float/   |} >- float_page
    ; {%otr| /contact/*/:int |} >- contact_page
    ]

(* Should output below: 

1: Float page. number : 100001.1
2: Product Page. Product Id : 100001
3: about page

*)
let () =
  [ Otr.match' router "/home/100001.1/"
  ; Otr.match' router "/home/100001/"
  ; Otr.match' router "/home/about"
  ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%d: %s\n" (i + 1) s
       | None -> Printf.printf "%d: None\n" (i + 1))

```
