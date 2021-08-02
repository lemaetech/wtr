## v2.0.0 2021-08-02 

- Convert `assert` based tests to `ppx_expect` tests
- Split README.md `mdx` test from tests
- BREAKING CHANGE: ppx `{%wtr| /home/about |}` now produces a function
  expecting a handler rather than a `uri`. Demos and tests have been updated to
  reflect this
- NEW : Add `Wtr.route` function to create a route. Routes now accept HTTP
  methods as part of a route. A new type `meth` has been added to represent all
  standard HTTP methods.
- NEW : Add `Wtr.pp_route` to pretty print routes.

## v1.0.0 2021-05-11 UK

- First release.
