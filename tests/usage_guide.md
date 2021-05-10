# Otr - OCaml typed router

## Getting started 

Otr is published in opam. You can install it by invoking ```opam install
otr```. 

Otr consists of the following two libraries:
- `otr` main library
- `otr.ppx` ppx library used to author `('a, 'b) Otr.path` type values. 

A sample dune file for an executable called `demo` which uses `otr` may look
like below.

```
(executable
 (name demo)
 (libraries otr)
 (preprocess
  (pps otr.ppx)))
```
## Specifying literal uri path

The following creates a uri path which matches `/home/about` exactly,

```ocaml
# let r = {%otr| /home/about |};;
val r : ('_weak1, '_weak1) Otr.path = <abstr>
```

```ocaml
# let router = Otr.(create [r >- "about page"]);;
val router : string Otr.t = <abstr>

# Otr.match' router "/home/about";;
- : string option = Some "about page"

# Otr.match' router "/home/about/";;
- : string option = None

# Otr.match' router "/home/About";;
- : string option = None

# Otr.match' router "/Home/about";;
- : string option = None
```
## Creating a route

A `'c Otr.route` value is created by applying an infix function `Otr.(>-)` to a uri path and its handler. `>-` has the following signature.

```ocaml
# Otr.(>-);;
- : ('a, 'b) Otr.path -> 'a -> 'b Otr.route = <fun>
```
Parameter `'a` above denotes a route handler. A route handler is dependent on what is specified in its corresponding uri path. 

An `Otr.path` value where only literal values are specified results in a
handler which immediately returns a value. 

```ocaml
# let r = Otr.({%otr| /home/about |} >- "about page");;
val r : string Otr.route = <abstr>
```

Where as an`Otr.path` where argument captures are specified results in a handler
which is a ocaml function.

```ocaml
# let r = Otr.({%otr| /home/;int |} >- fun (i: int) -> Printf.sprintf "int: %d" i);;
val r : (int -> string) Otr.route = <abstr>
```

