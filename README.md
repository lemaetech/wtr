# Wtr - Well Typed Router 

Wtr - A typed router for OCaml web applications.

[Wtr API](https://lemaetech.co.uk/wtr/wtr/Wtr/index.html)&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; [CoHttp Demo](https://github.com/lemaetech/wtr/blob/main/examples/cohttp.ml) &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; [Summer Demo](https://github.com/lemaetech/summer/blob/main/examples/echo_server.ml)

## Overview 

Wtr is typed router for OCaml web applications. 

- A trie based router. Route matching is efficient and fast.
- Route handlers are type-checked during compilation.
- Supports matching and capturing URI path components, eg `/home/about/:int`.
- Supports matching and capturing URI query parameters, eg `/home/about?q=:int&q1=hello`.
- Supports matching HTTP methods, eg `GET, POST, PUT, HEAD, DELETE` etc.
- Supports converting captured URI components to OCaml and custom user defined data types.
- `wtr.ppx` is used to specify uri values. If you know how to type URI path in a browser location, then you already know how to use `wtr`.
- Minimal learning overhead. `Wtr` is centered around 3 API calls, a ppx -`%wtr` - and your existing HTTP url/uri knowledge:
  - `Wtr.create` - creates a router - *'a Wtr.t* - from a list of `route` values.
  - `Wtr.match'` - matches a given uri path in a router.
  - `Wtr.decoder` - allows you to create a user defined decoder. *see `Fruit` module in the demo below.*

## Wtr Demo

```ocaml
# #require "wtr, wtr.ppx";;
```

```ocaml
module Fruit = struct
  type t = Apple | Orange | Pineapple

  let t : t Wtr.decoder =
    Wtr.decoder ~name:"fruit" ~decode:(function
      | "apple" -> Some Apple
      | "orange" -> Some Orange
      | "pineapple" -> Some Pineapple
      | _ -> None )
end

let about_page = "about page"
let prod_page i = "Int page. number : " ^ string_of_int i
let float_page f = "Float page. number : " ^ string_of_float f
let contact_page nm num = "Contact. Hi, " ^ nm ^ ". Num " ^ string_of_int num
let product1 name id q = Format.sprintf "Product1 %s. Id: %d. q = %b" name id q
let product2 name id = Format.sprintf "Product2 %s. Id: %d." name id
;;
let fruit_page = function
  | Fruit.Apple -> "Apples are juicy!"
  | Orange -> "Orange is a citrus fruit."
  | Pineapple -> "Pineapple has scaly skin"

let faq category_id _ =
  let category_name =
    match category_id with
    | 1 -> "products"
    | 2 -> "insurance"
    | 3 -> "returns"
    | _ -> "unknown"
  in
  "FAQ page for category : " ^ category_name

let router =
  Wtr.router      
      [ {%wtr| get,post,head,delete  ; /home/about/            |} about_page
      ; {%wtr| head,delete           ; /home/:int/             |} prod_page
      ; {%wtr| get,post              ; /home/:float/           |} float_page
      ; {%wtr| get; /contact/*/:int                            |} contact_page
      ; {%wtr| get; /product/:string?section=:int&q=:bool      |} product1
      ; {%wtr| get; /product/:string?section=:int&q1=yes       |} product2
      ; {%wtr| get; /fruit/:Fruit                              |} fruit_page
      ; {%wtr| GET; /faq/:int/**                               |} faq ]

let p () = 
  Printf.printf "\n====Router Match Results====\n" ;
  [ Wtr.match' `GET "/home/100001.1/" router
  ; Wtr.match' `DELETE "/home/100001/" router
  ; Wtr.match' `GET "/home/about/" router
  ; Wtr.match' `GET "/product/dyson350?section=233&q=true" router
  ; Wtr.match' `GET "/product/dyson350?section=2&q=false" router
  ; Wtr.match' `GET "/product/dyson350?section=2&q1=yes" router
  ; Wtr.match' `GET "/product/dyson350?section=2&q1=no" router
  ; Wtr.match' `GET "/fruit/apple" router
  ; Wtr.match' `GET "/fruit/orange" router
  ; Wtr.match' `GET "/fruit/pineapple" router
  ; Wtr.match' `GET "/fruit/guava" router
  ; Wtr.match' `GET "/faq/1/" router
  ; Wtr.match' `GET "/faq/1/whatever" router
  ; Wtr.match' `GET "/faq/2/whateasdfasdfasdf" router ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
       | None -> Printf.printf "%3d: None\n" (i + 1) );;
```

## Demo Output

```ocaml
# p () ;;
====Router Match Results====
  1: Float page. number : 100001.1
  2: Int page. number : 100001
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
 14: FAQ page for category : insurance
- : unit = ()
```
