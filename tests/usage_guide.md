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

## Using `otr.ppx`

Otr provides a ppx called `otr.ppx` in order to facilitate specifying uri route path and query components.

```ocaml
# let r = {%otr| /home/about |};;
val r : ('_weak1, '_weak1) Otr.path = <abstr>

```
