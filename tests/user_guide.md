# Wtr - Well Typed Router

Wtr - a typed router for OCaml web applications.

* [Getting started](#getting-started)
* [Specifying a Uri](#specifying-a-uri)
  + [TLDR - Quickly see some samples](#tldr---quickly-see-some-samples)
  + [Pretty Printing a URI](#pretty-printing-a-uri)
  + [Compile time validation](#compile-time-validation)
  + [Path](#path)
  + [Query](#query)
  + [Uri Components](#uri-components)
    - [Literal](#literal)
    - [Decoder](#decoder)
      * [User Defined Decoder](#user-defined-decoder)
    - [Full splat](#full-splat)
* [Creating a route](#creating-a-route)
* [Creating and matching a router](#creating-and-matching-a-router)

## Getting started 

Wtr is published in opam. You can install it by invoking ```opam install
wtr```. 

Wtr consists of the following two libraries:
- `wtr` main library
- `wtr.ppx` ppx library which provides the ppx extension `{%wtr| |}`.

A sample dune file for an executable called `demo` which uses `wtr` may look
like below.

```
(executable
 (name demo)
 (libraries wtr)
 (preprocess
  (pps wtr.ppx)))
```
## Specifying a Uri

A uri is created using a ppx in the form of `{%wtr| |}` or `[%wtr ""]`. It always starts with a **path** component followed by an optional **query component**.

### TLDR - Quickly see some samples

Some examples of specifying routes:

```ocaml
# let r = {%wtr| /home/about?a=:int&b=val |};;
val r : (int -> '_weak1, '_weak1) Wtr.uri = <abstr>

# let r = {%wtr| /home/:int/:float/:bool/:string |};;
val r : (int -> float -> bool -> string -> '_weak2, '_weak2) Wtr.uri =
  <abstr>

# let r = {%wtr| /home/about |};;
val r : ('_weak3, '_weak3) Wtr.uri = <abstr>

# let r = {%wtr| /home/about/ |};;
val r : ('_weak4, '_weak4) Wtr.uri = <abstr>

# let r = {%wtr| /home/*/contact |};;
val r : (string -> '_weak5, '_weak5) Wtr.uri = <abstr>

# let r = {%wtr| /home/about/** |};;
val r : ('_weak6, '_weak6) Wtr.uri = <abstr>

# let r = {%wtr| /home/:int/** |};;
val r : (int -> '_weak7, '_weak7) Wtr.uri = <abstr>
```

### Pretty Printing a URI

`Wtr.pp_uri` function can be used to pretty printing a `uri` value.

```ocaml
# Wtr.pp_uri;;
- : Format.formatter -> ('a, 'b) Wtr.uri -> unit = <fun>

# Wtr.pp_uri Format.std_formatter [%wtr "/home/about"];;
/home/about
- : unit = ()

# Wtr.pp_uri Format.std_formatter [%wtr "/home/:int/:string"];;
/home/:int/:string
- : unit = ()
```

### Compile time validation

```ocaml
# let r = {%wtr| home/about/ |};;
Line 1, characters 9-30:
Error: wtr: Uri path specification must start with '/'

# let r = {%wtr| /home/about/?a=b |};;
Line 1, characters 9-35:
Error: wtr: Invalid uri path specification. No tokens allowed after trailing
       '/' token

# let r = {%wtr| /home/about/**/abc |};;
Line 1, characters 9-37:
Error: wtr: Invalid uri path specification. No tokens allowed after full
       splat (**) token
```

### Path

Starts with `/` and may or may not end with `/`. Text following `/` is called a *path component*, i.e. a uri `/home/about` has two path components `home` and `about`.

Some examples of valid uri path:

```ocaml
# let about_page_uri = {%wtr| /home/about |};;
val about_page_uri : ('_weak8, '_weak8) Wtr.uri = <abstr>

# let product_detail_uri = {%wtr| /product/product1/details |};;
val product_detail_uri : ('_weak9, '_weak9) Wtr.uri = <abstr>

# let contact_uri = {%wtr| /home/contact/ |};;
val contact_uri : ('_weak10, '_weak10) Wtr.uri = <abstr>
```

Two paths with the same path components, such as `/home/about` and `/home/about/`, but with the only difference being the trailing `/` are not equal to each other. As such `wtr` matches them differently.

```ocaml

# [%wtr "/home/about"] = [%wtr "/home/about/"];;
- : bool = false
```

### Query  
  
Follows a uri path component and starts with a `?` character followed by one or more of a pair of key values. The key values are specified as `key=value` and are delimited with a `&` character, such as `?key=value&key2=value2`. Key and value are *query components*.

Query components always follow uri path component.

Uri values creation examples:
  
```ocaml
# let uri = {%wtr| /home/products/a?count=a&size=200 |};;
val uri : ('_weak11, '_weak11) Wtr.uri = <abstr>

# let r = {%wtr| /home/about |};;
val r : ('_weak12, '_weak12) Wtr.uri = <abstr>
```

### Uri Components

Both the *path component* and *query component* can be generally referred to as *uri component*. A *uri component* can be either a *literal*, a *decoder* or a *full splat*.

#### Literal 

A literal is matched by `wtr` exactly as it is given, eg. wtr matches uri '/home/about' as `home` and then `about`. 

#### Decoder
  
A decoder component starts with character **:** followed by a decoder name. It specifies the data decoding computation at the specified position.

For example, in uri `/home/:int` Wtr first matches the literal 'home' at position 1, followed by an attempt to decode value at position 2 with `int` decoder. If the decoding operation is successful then the component at position 2 is matched, else it is considered not matched. 

Wtr comes with a few built-in decoders:
- `int` - decodes OCaml *int* values
- `int32` - decodes OCaml *int32* values
- `int64` - decodes OCaml *int64* values
- `float` - decodes OCaml *float* values
- `bool` - decodes OCaml *bool* values
- `string` - decodes OCaml *string* values
- `*` - same as `string` decoder

Built-in decoder names start with *lowercase* letter.

Some examples of decoder usages:

```ocaml
# let r = {%wtr| /home/:int/:float/:bool |};;
val r : (int -> float -> bool -> '_weak13, '_weak13) Wtr.uri = <abstr>
```
You can use decoder components in Query *value* component position:

```ocaml
# let r = [%wtr "/home/contact?name=:string&number=:int"];;
val r : (string -> int -> '_weak14, '_weak14) Wtr.uri = <abstr>
```

##### User Defined Decoder 

In addition to the built-in decoders, we can also define and use a user defined decoder. User defined decoder are specified as a module which conforms to the following signature:

```ocaml
module type Decoder = sig
  type t

  val t : t Wtr.decoder
end
```
User defined decoder names correspond to a *module name*.

Here is how we can define a user defined decoder called `Fruit`. Note the decoder name and the module name match. 

```ocaml
# module Fruit = struct
    type t =
      | Apple
      | Orange
      | Pineapple

    let t : t Wtr.decoder =
      Wtr.create_decoder ~name:"fruit" ~decode:(function
        | "apple" -> Some Apple
        | "orange" -> Some Orange
        | "pineapple" -> Some Pineapple
        | _ -> None)
  end;;
module Fruit :
  sig type t = Apple | Orange | Pineapple val t : t Wtr.decoder end
```

Here is how we can use the `Fruit` decoder:

```ocaml
# let r = {%wtr| /home/:Fruit |};;
val r : (Fruit.t -> '_weak15, '_weak15) Wtr.uri = <abstr>
```

#### Full splat

A full splat uri component specifies that Wtr should consider the uri matched after it is encountered. The rest of uri values are ignored.

Here, Wtr matches `/home/about`, `/home/contact` and `/home/product/product2` urls.

```ocaml
# let full_splat = [%wtr "/home/**"];;
val full_splat : ('_weak16, '_weak16) Wtr.uri = <abstr>
```

## Creating a route

A `'c Wtr.route` value is created by applying an infix function `Wtr.(>-)` to a uri and its handler. `>-` has the following signature.

```ocaml
# Wtr.(>-);;
- : ('a, 'b) Wtr.uri -> 'a -> 'b Wtr.route = <fun>
```
Parameter `'a` above denotes a route handler. A route handler is dependent on what is specified in its corresponding uri. 

An `Wtr.uri` value where only literal values are specified results in a handler which immediately returns a value. 

```ocaml
# let r = Wtr.({%wtr| /home/about |} >- "about page");;
val r : string Wtr.route = <abstr>
```

Whereas an`Wtr.uri` where decoders are specified results in a handler which is a ocaml function. The parameters of the function match the order in which the decoders are specified.

```ocaml
# let r = Wtr.({%wtr| /home/:int |} >- fun (i: int) -> Printf.sprintf "int: %d" i);;
val r : string Wtr.route = <abstr>
```

## Creating and matching a router

A route is created by applying function a list of `route` values to `Wtr.create`. The function signature of `Wtr.create` is as follows:

```ocaml
# Wtr.create;;
- : 'a Wtr.route list -> 'a Wtr.t = <fun>
```

```ocaml
# let router = Wtr.(create [{%wtr| /home/about |} >- "about page"]);;
val router : string Wtr.t = <abstr>
```
Matching is peformed by applying a `router` and a `uri` value. `Wtr.match`
returns `Some a` if the given `uri` matches one of the routes in `router`. `a`
represents the value computed by the route handler.

```ocaml
# Wtr.match';;
- : 'a Wtr.t -> string -> 'a option = <fun>
```

```ocaml
# Wtr.match' router "/home/about";;
- : string option = Some "about page"

# Wtr.match' router "/home/about/";;
- : string option = None

# Wtr.match' router "/home/About";;
- : string option = None

# Wtr.match' router "/Home/about";;
- : string option = None
```
