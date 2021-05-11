# Otr - OCaml typed router

## Getting started 

Otr is published in opam. You can install it by invoking ```opam install
otr```. 

Otr consists of the following two libraries:
- `otr` main library
- `otr.ppx` ppx library which provides the ppx extension `{%otr||}`.

A sample dune file for an executable called `demo` which uses `otr` may look
like below.

```
(executable
 (name demo)
 (libraries otr)
 (preprocess
  (pps otr.ppx)))
```
## Specifying a uri

A uri is created using a ppx in the form of `{%otr| |}` or `[%otr ""]`. It always starts with a **path** component followed by an optional **query component**.

* Path

  Starts with `/` and may or may not end with `/`. Text following `/` is called a *path component*, i.e. a path `/home/about` has two path components `home` and `about`.
  
  Some examples of valid uri path:

  __Note__ Two paths with same path components but with one ending on `/` and the other not ending with `/` are not the same, i.e. `/home/about` and `/home/about/` are not equal to each other.

```ocaml
  # let about_page_uri = {%otr| /home/about |};;
  val about_page_uri : ('_weak1, '_weak1) Otr.uri = <abstr>

  # let product_detail_uri = {%otr| /product/product1/details |};;
  val product_detail_uri : ('_weak2, '_weak2) Otr.uri = <abstr>

  # let contact_uri = {%otr| /home/contact/ |};;
  val contact_uri : ('_weak3, '_weak3) Otr.uri = <abstr>
```

* Query  
  
  Follows a uri path component and starts with a `?` character followed by one or more of a pair of key values. The key values are specified as `key=value` and are delimited with a `&` character, such as `?key=value&key2=value2`. Key and values are called the *query components*.

  Query components always follow uri path component.

Some examples of uri values:
  
```ocaml
# let uri = {%otr| /home/products/a?count=a&size=200 |};;
val uri : ('_weak4, '_weak4) Otr.uri = <abstr>

# let r = {%otr| /home/about |};;
val r : ('_weak5, '_weak5) Otr.uri = <abstr>
```

Uri path can contain argument captures. They specify the data type of the decoding operation. The specification starts with `:` followed by the capture name.

The path below captures a decoded value of OCaml type `int` after matching literal `home`.

```ocaml
# let r = {%otr| /home/:int |};;
val r : (int -> '_weak6, '_weak6) Otr.uri = <abstr>
```

Lastly, the uri to be matched can also specify query params to be matched.
Query param captures and literals needs to be specified after the `=` character.

```ocaml
# let r = {%otr| /home/about?a=:int&b=val |};;
val r : (int -> '_weak7, '_weak7) Otr.uri = <abstr>
```

### Standard argument captures
    
`otr` provides the following captures as a default:
- `int`
- `int32`
- `int64`
- `float`
- `bool`
- `string`

A sample usage:

```ocaml
# let r = {%otr| /home/:int/:float/:bool/:string |};;
val r : (int -> float -> bool -> string -> '_weak8, '_weak8) Otr.uri =
  <abstr>
```
### User defined argument captures 

In addition to the standard argument captures, otr allows a user to define
custom user defined argument captures. User defined argument captures are
defined in a module. The argument capture name is a fully qualified module
name. 

A sample user defined capture which defines `:Fruit` argument capture can be
deinfed as such,
```ocaml
# module Fruit = struct
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
  end;;
module Fruit : sig type t = Apple | Orange | Pineapple val t : t Otr.arg end

# let r = {%otr| /home/:Fruit |};;
val r : (Fruit.t -> '_weak9, '_weak9) Otr.uri = <abstr>
```

## Creating a route

A `'c Otr.route` value is created by applying an infix function `Otr.(>-)` to a uri path and its handler. `>-` has the following signature.

```ocaml
# Otr.(>-);;
- : ('a, 'b) Otr.uri -> 'a -> 'b Otr.route = <fun>
```
Parameter `'a` above denotes a route handler. A route handler is dependent on what is specified in its corresponding uri path. 

An `Otr.path` value where only literal values are specified results in a
handler which immediately returns a value. 

```ocaml
# let r = Otr.({%otr| /home/about |} >- "about page");;
val r : string Otr.route = <abstr>
```

Whereas an`Otr.path` where argument captures are specified results in a handler
which is a ocaml function.

```ocaml
# let r = Otr.({%otr| /home/:int |} >- fun (i: int) -> Printf.sprintf "int: %d" i);;
val r : string Otr.route = <abstr>
```

## Creating and matching a router

A route is created by applying function a list of `route` values to `Otr.create`. The function signature of `Otr.create` is as follows:

```ocaml
# Otr.create;;
- : 'a Otr.route list -> 'a Otr.t = <fun>
```

```ocaml
# let router = Otr.(create [{%otr| /home/about |} >- "about page"]);;
val router : string Otr.t = <abstr>
```
Matching is peformed by applying a `router` and a `uri` value. `Otr.match`
returns `Some a` if the given `uri` matches one of the routes in `router`. `a`
represents the value computed by the route handler.

```ocaml
# Otr.match';;
- : 'a Otr.t -> string -> 'a option = <fun>
```

```ocaml
# Otr.match' router "/home/about";;
- : string option = Some "about page"

# Otr.match' router "/home/about/";;
- : string option = None

# Otr.match' router "/home/About";;
- : string option = None

# Otr.match' router "/Home/about";;
- : string option = None
```
