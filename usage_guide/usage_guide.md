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

A uri is created using a ppx in the form of `{%otr| |}` or `[%otr ""]`. It consits of two main 
components,

* Path

  A uri starts with a path component and is denoted by a `/` character. It 

* Query  
delimited via the forward slash character `/` in the uri path and via the characters `&` and `=` 
in the query components of the uri.

The path components in the uri must always start with `/`. The query components start with `?` character after the path components. 


```ocaml
# let uri = {%otr| /home/products/a?count=a&size=200 |};;
val uri : ('_weak1, '_weak1) Otr.uri = <abstr>
```

The uri which represents `/home/about` has exactly two literal path components, `home` and `about`. 

```ocaml
# let r = {%otr| /home/about |};;
val r : ('_weak2, '_weak2) Otr.uri = <abstr>
```

Uri path can contain argument captures. They specify the data type of the decoding operation. The specification starts with `:` followed by the capture name.

The path below captures a decoded value of OCaml type `int` after matching literal `home`.

```ocaml
# let r = {%otr| /home/:int |};;
val r : (int -> '_weak3, '_weak3) Otr.uri = <abstr>
```

Lastly, the uri to be matched can also specify query params to be matched.
Query param captures and literals needs to be specified after the `=` character.

```ocaml
# let r = {%otr| /home/about?a=:int&b=val |};;
val r : (int -> '_weak4, '_weak4) Otr.uri = <abstr>
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
val r : (int -> float -> bool -> string -> '_weak5, '_weak5) Otr.uri =
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
val r : (Fruit.t -> '_weak6, '_weak6) Otr.uri = <abstr>
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
