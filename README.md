# Otr - OCaml Typed Router 
*(Unreleased)*

A typed router for OCaml web applications. 

- A trie based router. Route matching is efficient and fast.
- Route handlers are type-checked during compilation.
- Supports matching and capturing URI path components, eg `/home/about/:int`.
- Supports matching and capturing URI query parameters, eg `/home/about?q=:int&q1=hello`.
- Supports converting captured URI components to OCaml and custom user defined data types. OCaml data types supported are:
  - `int`
  - `float`
  - `bool`
  - `string` 
- Route URI path is specified via a `ppxlib` based modern ppx -`otr.ppx`. If you know how to type URI path in a browser location, then you already know how to use `otr`.
- Minimal learning overhead. `Otr` is centered around just four API calls and a ppx - `otr.ppx`. 
  - `{%otr| /home/products/:int |}` - creates a `path` and and infix function `>-` creates a `route`. 
  - `Otr.create` - creates a router from a list of `route` values
  - `Otr.match'` - matches a given uri path in a router.
  - `Otr.create_arg` - allows you to associate, capture and convert uri components to a user defined datatype. *Note* User defined capture variable are OCaml module names.

__A Demo of the features__

```ocaml
open! Otr
open! Printf

(* User defined data type. *)
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
      | _ -> None )
end

(* create a router *)
let rec router () =
  create
    [ {%otr| /home/about                           |} >- "about page"
    ; {%otr| /home/:int/                           |} >- prod_page
    ; {%otr| /home/:float/                         |} >- float_page
    ; {%otr| /contact/*/:int                       |} >- contact_page
    ; {%otr| /product/:string?section=:int&q=:bool |} >- product1
    ; {%otr| /product/:string?section=:int&q1=yes  |} >- product2
    ; {%otr| /fruit/:Fruit                         |} >- fruit_page ]

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

let () =
  let router = router () in
  [ Otr.match' router "/home/100001.1/"; Otr.match' router "/home/100001/"
  ; Otr.match' router "/home/about"
  ; Otr.match' router "/product/dyson350?section=233&q=true"
  ; Otr.match' router "/product/dyson350?section=2&q=false"
  ; Otr.match' router "/product/dyson350?section=2&q1=yes"
  ; Otr.match' router "/product/dyson350?section=2&q1=no"
  ; Otr.match' router "/fruit/apple"; Otr.match' router "/fruit/orange"
  ; Otr.match' router "/fruit/pineapple"; Otr.match' router "/fruit/guava" ]
  |> List.iteri (fun i -> function
       | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
       | None -> Printf.printf "%3d: None\n" (i + 1) )

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
