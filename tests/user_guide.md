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
# #require "wtr, wtr.ppx";;
```

```ocaml
# let r = {%wtr| /home/about?a=:int&b=val |};;
Line 1, characters 9-43:
Error: Invalid wtr: /home/about?a=:int&b=val. Valid wtr is: [HTTP methods
       separated by comma (,)] ; [URI]

# let r = {%wtr| /home/:int/:float/:bool/:string |};;
Line 1, characters 9-50:
Error: Invalid wtr: /home/:int/:float/:bool/:string. Valid wtr is: [HTTP
       methods separated by comma (,)] ; [URI]

# let r = {%wtr| /home/about |};;
Line 1, characters 9-30:
Error: Invalid wtr: /home/about. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# let r = {%wtr| /home/about/ |};;
Line 1, characters 9-31:
Error: Invalid wtr: /home/about/. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# let r = {%wtr| /home/*/contact |};;
Line 1, characters 9-34:
Error: Invalid wtr: /home/*/contact. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# let r = {%wtr| /home/about/** |};;
Line 1, characters 9-33:
Error: Invalid wtr: /home/about/**. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# let r = {%wtr| /home/:int/** |};;
Line 1, characters 9-32:
Error: Invalid wtr: /home/:int/**. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
```

### Pretty Printing a URI

`Wtr.pp_uri` function can be used to pretty printing a `uri` value.

```ocaml
# Wtr.pp_route;;
- : Format.formatter -> 'b Wtr.route -> unit = <fun>

# Wtr.pp_route Format.std_formatter ([%wtr "/home/about"] () );;
Line 1, characters 36-56:
Error: Invalid wtr: /home/about. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# Wtr.pp_route Format.std_formatter ([%wtr "/home/:int/:string"] (fun _ _ -> ()) );;
Line 1, characters 36-63:
Error: Invalid wtr: /home/:int/:string. Valid wtr is: [HTTP methods separated
       by comma (,)] ; [URI]
```

### Compile time validation

```ocaml
# let r = {%wtr| home/about/ |};;
Line 1, characters 9-30:
Error: Invalid wtr: home/about/. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# let r = {%wtr| /home/about/?a=b |};;
Line 1, characters 9-35:
Error: Invalid wtr: /home/about/?a=b. Valid wtr is: [HTTP methods separated
       by comma (,)] ; [URI]

# let r = {%wtr| /home/about/**/abc |};;
Line 1, characters 9-37:
Error: Invalid wtr: /home/about/**/abc. Valid wtr is: [HTTP methods separated
       by comma (,)] ; [URI]
```

### Path

Starts with `/` and may or may not end with `/`. Text following `/` is called a *path component*, i.e. a uri `/home/about` has two path components `home` and `about`.

Some examples of valid uri path:

```ocaml
# let about_page_uri = {%wtr| /home/about |};;
Line 1, characters 22-43:
Error: Invalid wtr: /home/about. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]

# let product_detail_uri = {%wtr| /product/product1/details |};;
Line 1, characters 26-61:
Error: Invalid wtr: /product/product1/details. Valid wtr is: [HTTP methods
       separated by comma (,)] ; [URI]

# let contact_uri = {%wtr| /home/contact/ |};;
Line 1, characters 19-43:
Error: Invalid wtr: /home/contact/. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
```

### Query  
  
Follows a uri path component and starts with a `?` character followed by one or more of a pair of key values. The key values are specified as `key=value` and are delimited with a `&` character, such as `?key=value&key2=value2`. Key and value are *query components*.

Query components always follow uri path component.

Uri values creation examples:
  
```ocaml
# let uri = {%wtr| /home/products/a?count=a&size=200 |};;
Line 1, characters 11-54:
Error: Invalid wtr: /home/products/a?count=a&size=200. Valid wtr is: [HTTP
       methods separated by comma (,)] ; [URI]

# let r = {%wtr| /home/about |};;
Line 1, characters 9-30:
Error: Invalid wtr: /home/about. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
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
Line 1, characters 9-42:
Error: Invalid wtr: /home/:int/:float/:bool. Valid wtr is: [HTTP methods
       separated by comma (,)] ; [URI]
```
You can use decoder components in Query *value* component position:

```ocaml
# let r = [%wtr "/home/contact?name=:string&number=:int"];;
Line 1, characters 9-56:
Error: Invalid wtr: /home/contact?name=:string&number=:int. Valid wtr is:
       [HTTP methods separated by comma (,)] ; [URI]
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
Line 1, characters 9-31:
Error: Invalid wtr: /home/:Fruit. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
```

#### Full splat

A full splat uri component specifies that Wtr should consider the uri matched after it is encountered. The rest of uri values are ignored.

Here, Wtr matches `/home/about`, `/home/contact` and `/home/product/product2` urls.

```ocaml
# let full_splat = [%wtr "/home/**"];;
Line 1, characters 18-35:
Error: Invalid wtr: /home/**. Valid wtr is: [HTTP methods separated by comma
       (,)] ; [URI]
```

## Creating a route

A `'c Wtr.route` value is created by applying an infix function `Wtr.(>-)` to a uri and its handler. `>-` has the following signature.

```ocaml
# Wtr.(>-);;
Line 1, characters 1-9:
Error: Unbound value Wtr.>-
```
Parameter `'a` above denotes a route handler. A route handler is dependent on what is specified in its corresponding uri. 

An `Wtr.uri` value where only literal values are specified results in a handler which immediately returns a value. 

```ocaml
# let r = Wtr.({%wtr| /home/about |} "about page");;
Line 1, characters 14-35:
Error: Invalid wtr: /home/about. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
```

Whereas an`Wtr.uri` where decoders are specified results in a handler which is a ocaml function. The parameters of the function match the order in which the decoders are specified.

```ocaml
# let r = Wtr.({%wtr| /home/:int |} (fun (i: int) -> Printf.sprintf "int: %d" i));;
Line 1, characters 14-34:
Error: Invalid wtr: /home/:int. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
```

## Creating and matching a router

A route is created by applying function a list of `route` values to `Wtr.create`. The function signature of `Wtr.create` is as follows:

```ocaml
# Wtr.create;;
- : 'a Wtr.route list list -> 'a Wtr.t = <fun>
```

```ocaml
# let router = Wtr.(create [{%wtr| /home/about |} "about page"]);;
Line 1, characters 27-48:
Error: Invalid wtr: /home/about. Valid wtr is: [HTTP methods separated by
       comma (,)] ; [URI]
```
Matching is peformed by applying a `router` and a `uri` value. `Wtr.match`
returns `Some a` if the given `uri` matches one of the routes in `router`. `a`
represents the value computed by the route handler.

```ocaml
# Wtr.match';;
- : Wtr.method' -> string -> 'a Wtr.t -> 'a option = <fun>
```

```ocaml
# Wtr.match' router "/home/about";;
Line 1, characters 12-18:
Error: Unbound value router

# Wtr.match' router "/home/about/";;
Line 1, characters 12-18:
Error: Unbound value router

# Wtr.match' router "/home/About";;
Line 1, characters 12-18:
Error: Unbound value router

# Wtr.match' router "/Home/about";;
Line 1, characters 12-18:
Error: Unbound value router
```
