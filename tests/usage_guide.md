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
