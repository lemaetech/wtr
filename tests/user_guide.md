# Otr - OCaml typed router

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

Otr is published in opam. You can install it by invoking ```opam install
otr```. 

Otr consists of the following two libraries:
- `otr` main library
- `otr.ppx` ppx library which provides the ppx extension `{%otr| |}`.

A sample dune file for an executable called `demo` which uses `otr` may look
like below.

```
(executable
 (name demo)
 (libraries otr)
 (preprocess
  (pps otr.ppx)))
```
## Specifying a Uri

A uri is created using a ppx in the form of `{%otr| |}` or `[%otr ""]`. It always starts with a **path** component followed by an optional **query component**.

### TLDR - Quickly see some samples

Some examples of specifying routes:

```ocaml
# let r = {%otr| /home/about?a=:int&b=val |};;
val r : (int -> '_weak1, '_weak1) Otr.uri = <abstr>

# let r = {%otr| /home/:int/:float/:bool/:string |};;
val r : (int -> float -> bool -> string -> '_weak2, '_weak2) Otr.uri =
  <abstr>

# let r = {%otr| /home/about |};;
val r : ('_weak3, '_weak3) Otr.uri = <abstr>

# let r = {%otr| /home/about/ |};;
val r : ('_weak4, '_weak4) Otr.uri = <abstr>

# let r = {%otr| /home/*/contact |};;
val r : (string -> '_weak5, '_weak5) Otr.uri = <abstr>

# let r = {%otr| /home/about/** |};;
val r : ('_weak6, '_weak6) Otr.uri = <abstr>

# let r = {%otr| /home/:int/** |};;
val r : (int -> '_weak7, '_weak7) Otr.uri = <abstr>
```

### Pretty Printing a URI

`Otr.pp_uri` function can be used to pretty printing a `uri` value.

```ocaml
# Otr.pp_uri;;
- : Format.formatter -> ('a, 'b) Otr.uri -> unit = <fun>

# Otr.pp_uri Format.std_formatter [%otr "/home/about"];;
/home/about
- : unit = ()

# Otr.pp_uri Format.std_formatter [%otr "/home/:int/:string"];;
/home/:int/:string
- : unit = ()
```

### Compile time validation

```ocaml
# let r = {%otr| home/about/ |};;
Line 1, characters 9-30:
Error: otr: Uri path specification must start with '/'

# let r = {%otr| /home/about/?a=b |};;
Line 1, characters 9-35:
Error: otr: Invalid uri path specification. No tokens allowed after trailing
       '/' token

# let r = {%otr| /home/about/**/abc |};;
Line 1, characters 9-37:
Error: otr: Invalid uri path specification. No tokens allowed after full
       splat (**) token
```

### Path

Starts with `/` and may or may not end with `/`. Text following `/` is called a *path component*, i.e. a uri `/home/about` has two path components `home` and `about`.

Some examples of valid uri path:

```ocaml
# let about_page_uri = {%otr| /home/about |};;
val about_page_uri : ('_weak8, '_weak8) Otr.uri = <abstr>

# let product_detail_uri = {%otr| /product/product1/details |};;
val product_detail_uri : ('_weak9, '_weak9) Otr.uri = <abstr>

# let contact_uri = {%otr| /home/contact/ |};;
val contact_uri : ('_weak10, '_weak10) Otr.uri = <abstr>
```

Two paths with the same path components, such as `/home/about` and `/home/about/`, but with the only difference being the trailing `/` are not equal to each other. As such `otr` matches them differently.

```ocaml

# [%otr "/home/about"] = [%otr "/home/about/"];;
- : bool = false
```

### Query  
  
Follows a uri path component and starts with a `?` character followed by one or more of a pair of key values. The key values are specified as `key=value` and are delimited with a `&` character, such as `?key=value&key2=value2`. Key and value are *query components*.

Query components always follow uri path component.

Uri values creation examples:
  
```ocaml
# let uri = {%otr| /home/products/a?count=a&size=200 |};;
val uri : ('_weak11, '_weak11) Otr.uri = <abstr>

# let r = {%otr| /home/about |};;
val r : ('_weak12, '_weak12) Otr.uri = <abstr>
```

### Uri Components

Both the *path component* and *query component* can be generally referred to as *uri component*. A *uri component* can be either a *literal*, a *decoder* or a *full splat*.

#### Literal 

A literal is matched by `otr` exactly as it is given, eg. otr matches uri '/home/about' as `home` and then `about`. 

#### Decoder
  
A decoder component starts with character **:** followed by a decoder name. It specifies the data decoding computation at the specified position.

For example, in uri `/home/:int` Otr first matches the literal 'home' at position 1, followed by an attempt to decode value at position 2 with `int` decoder. If the decoding operation is successful then the component at position 2 is matched, else it is considered not matched. 

Otr comes with a few built-in decoders:
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
# let r = {%otr| /home/:int/:float/:bool |};;
val r : (int -> float -> bool -> '_weak13, '_weak13) Otr.uri = <abstr>
```
You can use decoder components in Query *value* component position:

```ocaml
# let r = [%otr "/home/contact?name=:string&number=:int"];;
val r : (string -> int -> '_weak14, '_weak14) Otr.uri = <abstr>
```

##### User Defined Decoder 

In addition to the built-in decoders, we can also define and use a user defined decoder. User defined decoder are specified as a module which conforms to the following signature:

```ocaml
module type Decoder = sig
  type t

  val t : t Otr.decoder
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

    let t : t Otr.decoder =
      Otr.create_decoder ~name:"fruit" ~decode:(function
        | "apple" -> Some Apple
        | "orange" -> Some Orange
        | "pineapple" -> Some Pineapple
        | _ -> None)
  end;;
module Fruit :
  sig type t = Apple | Orange | Pineapple val t : t Otr.decoder end
```

Here is how we can use the `Fruit` decoder:

```ocaml
# let r = {%otr| /home/:Fruit |};;
val r : (Fruit.t -> '_weak15, '_weak15) Otr.uri = <abstr>
```

#### Full splat

A full splat uri component specifies that Otr should consider the uri matched after it is encountered. The rest of uri values are ignored.

Here, Otr matches `/home/about`, `/home/contact` and `/home/product/product2` urls.

```ocaml
# let full_splat = [%otr "/home/**"];;
val full_splat : ('_weak16, '_weak16) Otr.uri = <abstr>
```

## Creating a route

A `'c Otr.route` value is created by applying an infix function `Otr.(>-)` to a uri and its handler. `>-` has the following signature.

```ocaml
# Otr.(>-);;
- : ('a, 'b) Otr.uri -> 'a -> 'b Otr.route = <fun>
```
Parameter `'a` above denotes a route handler. A route handler is dependent on what is specified in its corresponding uri. 

An `Otr.uri` value where only literal values are specified results in a handler which immediately returns a value. 

```ocaml
# let r = Otr.({%otr| /home/about |} >- "about page");;
val r : string Otr.route = <abstr>
```

Whereas an`Otr.uri` where decoders are specified results in a handler which is a ocaml function. The parameters of the function match the order in which the decoders are specified.

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
