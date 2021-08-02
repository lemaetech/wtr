# Wtr - Well Typed Router 

Wtr - A typed router for OCaml web applications.

[User Guide](https://github.com/lemaetech/wtr/blob/main/tests/user_guide.md) &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;[Wtr API](https://lemaetech.co.uk/wtr/wtr/Wtr/index.html)&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; [CoHttp Demo](https://github.com/lemaetech/wtr/blob/main/examples/cohttp.ml)

## Overview 

Wtr is typed router for OCaml web applications. 

- A trie based router. Route matching is efficient and fast.
- Route handlers are type-checked during compilation.
- Supports matching and capturing URI path components, eg `/home/about/:int`.
- Supports matching and capturing URI query parameters, eg `/home/about?q=:int&q1=hello`.
- Supports converting captured URI components to OCaml and custom user defined data types.
- `wtr.ppx` is used to specify uri values. If you know how to type URI path in a browser location, then you already know how to use `wtr`.
- Minimal learning overhead. `Wtr` is centered around just four API calls, a ppx and your existing HTTP url/uri knowledge:
  - `{%wtr| /home/products/:int |}` - creates a `uri` value using `%wtr` ppx extension.
  - `>-` - an inifx function - creates a `route` given a `uri` value and a route handler. A route handler is just a normal OCaml function.
  - `Wtr.create` - creates a router - *'a Wtr.t* - from a list of `route` values.
  - `Wtr.match'` - matches a given uri path in a router.
  - `Wtr.create_decoder` - allows you to create a user defined decoder. *see `Fruit` module in the demo below.*

## Wtr Demo

```ocaml
# #require "wtr, wtr.ppx";;
```

```ocaml
open! Wtr;;
open! Printf;;

(* User defined decoder. *)
module Fruit = struct
  type t =
    | Apple
    | Orange
    | Pineapple

  let t : t Wtr.decoder =
    Wtr.create_decoder ~name:"fruit" ~decode:(function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None )
end

let rec router () =
  create
    [ {%wtr| /home/about                           |} >- "about page"
    ; {%wtr| /home/:int/                           |} >- prod_page
    ; {%wtr| /home/:float/                         |} >- float_page
    ; {%wtr| /contact/*/:int                       |} >- contact_page
    ; {%wtr| /product/:string?section=:int&q=:bool |} >- product1
    ; {%wtr| /product/:string?section=:int&q1=yes  |} >- product2
    ; {%wtr| /fruit/:Fruit                         |} >- fruit_page
    ; {%wtr| /faq/:int/**                          |} >- faq
    ]

(* route handlers. *)
and prod_page i = "Product Page. Product Id : " ^ string_of_int i
and float_page f = "Float page. number : " ^ string_of_float f
and contact_page nm num = "Contact. Hi, " ^ nm ^ ". Num " ^ string_of_int num
and product1 name id q = sprintf "Product1 %s. Id: %d. q = %b" name id q
and product2 name id = sprintf "Product2 %s. Id: %d." name id

and fruit_page = function
  | Fruit.Apple -> "Apples are juicy!"
  | Orange -> "Orange is a citrus fruit."
  | Pineapple -> "Pineapple has scaly skin"

and faq category_id =
  let category_name =
    match category_id with
    | 1 -> "products"
    | 2 -> "insurance"
    | 3 -> "returns"
    | _ -> "unknown"
  in
  "FAQ page for category : " ^ category_name

let () =
  let router = router () in
  [ Wtr.match' router "/home/100001.1/"; Wtr.match' router "/home/100001/"
  ; Wtr.match' router "/home/about"
  ; Wtr.match' router "/product/dyson350?section=233&q=true"
  ; Wtr.match' router "/product/dyson350?section=2&q=false"
  ; Wtr.match' router "/product/dyson350?section=2&q1=yes"
  ; Wtr.match' router "/product/dyson350?section=2&q1=no"
  ; Wtr.match' router "/fruit/apple"; Wtr.match' router "/fruit/orange"
  ; Wtr.match' router "/fruit/pineapple"; Wtr.match' router "/fruit/guava" 
  ; Wtr.match' router "/faq/1/"
  ; Wtr.match' router "/faq/1/whatever"
  ; Wtr.match' router "/faq/2/whateasdfasdfasdf"
  ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
       | None -> Printf.printf "%3d: None\n" (i + 1) );;
```
```mdx-error
Line 19, characters 9-56:
Warning 5 [ignored-partial-application]: this function application is partial,
maybe some arguments are missing.
Line 19, characters 9-56:
Error: This expression has type 'a -> 'a route
       but an expression was expected of type ('b, 'c) uri
```

### Output of the Demo

```
  1: Float page. number : 100001.1
  2: Product Page. Product Id : 100001
  3: about page
  4: Product1 dyson350. Id: 233. q = true
  5: Product1 dyson350. Id: 2. q = false
  6: Product2 dyson350. Id: 2.
  7: None
  8: Apples are juicy!
  9: Orange is a citrus fruit.
 10: Pineapple has scaly skin
 11: None
 12: FAQ page for category : products
 13: FAQ page for category : products
 14: FAQ page for category : insuranc
```
