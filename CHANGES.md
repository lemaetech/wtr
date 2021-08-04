## v2.0.0 2021-08-02 

- Convert `assert` based tests to `ppx_expect` tests
- Split README.md `mdx` test from tests
- BREAKING CHANGE: ppx `{%wtr| /home/about |}` now produces a function
  expecting a handler rather than a `uri`. Demos and tests have been updated to
  reflect this change.
- BREAKING CHANGE: remove functions `pp_uri` and `>-`.
- BREAKING CHANGE: `create` function now mandates `[route list list]` to create
  a router. Hopefully this should not cause much breakage since the `%wtr` ppx 
  produces this value.
- NEW : `%wtr` ppx now allows specifying HTTP methods along with the uri route. A new 
  type `meth` has been added to represent all standard HTTP methods.
- NEW : Add `Wtr.pp_route`, `Wtr.pp_method'` and `Wtr.pp` to pretty print a route, method 
  and a router respectively.

## v1.0.0 2021-05-11 UK

- First release.
