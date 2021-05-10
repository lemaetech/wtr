# Otr - OCaml Typed Router 
*(Unreleased)*

A typed router for OCaml web applications. 

- A trie based router.
- Route handlers are type-checked during compilation.
- Supports matching and capturing URI path components, eg `/home/about/:int`.
- Supports matching and capturing on query parameters, eg `/home/about?q=:int&q1=hello`.
- Supports converting URI to OCaml data types as well and custom defined data types. For now conversion to `int`, `float`, `bool`, and `string` are supported. 
- Provides a ppx for API ergonomics. Specify uri path as it appears in the browser, eg. `/home/about`, `/home/:int`.

__A Demo of the features__

```ocaml

open! Otr

let prod_page i = "Product Page. Product Id : " ^ string_of_int i

let float_page f = "Float page. number : " ^ string_of_float f

let contact_page name number =
  "Contact page. Hi, " ^ name ^ ". Number " ^ string_of_int number

let product_detail name section_id q =
  Printf.sprintf "Product detail - %s. Section: %d. Display questions? %b" name
    section_id q

let product2 name section_id =
  Printf.sprintf "Product detail 2 - %s. Section: %d." name section_id

module Fruit = struct
  type t =
    | Apple
    | Orange
    | Pineapple

  let t : t Otr.arg =
    Otr.create_arg ~name:"fruit" ~decode:(function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None)
end

let fruit_page = function
  | Fruit.Apple -> "Apples are juicy!"
  | Orange -> "Orange is a citrus fruit."
  | Pineapple -> "Pineapple has scaly skin"

let router =
  create
    [ {%otr| /home/about                           |} >- "about page"
    ; {%otr| /home/:int/                           |} >- prod_page
    ; {%otr| /home/:float/                         |} >- float_page
    ; {%otr| /contact/*/:int                       |} >- contact_page
    ; {%otr| /product/:string?section=:int&q=:bool |} >- product_detail
    ; {%otr| /product/:string?section=:int&q1=yes  |} >- product2
    ; {%otr| /fruit/:Fruit                         |} >- fruit_page
    ]

let () =
  [ Otr.match' router "/home/100001.1/"
  ; Otr.match' router "/home/100001/"
  ; Otr.match' router "/home/about"
  ; Otr.match' router "/product/dyson350?section=233&q=true"
  ; Otr.match' router "/product/dyson350?section=2&q=false"
  ; Otr.match' router "/product/dyson350?section=2&q1=yes"
  ; Otr.match' router "/product/dyson350?section=2&q1=no"
  ; Otr.match' router "/fruit/apple"
  ; Otr.match' router "/fruit/orange"
  ; Otr.match' router "/fruit/pineapple"
  ; Otr.match' router "/fruit/guava"
  ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
       | None -> Printf.printf "%3d: None\n" (i + 1))

```
__Running the demo__
```dune exec examples/demo.exe```

It should print below in the terminal.
```
  1: Float page. number : 100001.1
  2: Product Page. Product Id : 100001
  3: about page
  4: Product detail - dyson350. Section: 233. Display questions? true
  5: Product detail - dyson350. Section: 2. Display questions? false
  6: Product detail 2 - dyson350. Section: 2.
  7: None
  8: Apples are juicy!
  9: Orange is a citrus fruit.
 10: Pineapple has scaly skin
 11: None

```
