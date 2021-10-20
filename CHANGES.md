## v3.0.0 [2021-10-20] 

- BREAKING CHANGE A splat operator ** now expects a last string paramter
  in the handler. The parameter holds the remaining url matched by the splat operator
- BREAKING CHANGE create_decoder has been renamed to arg
- BREAKING CHANGE create has been renamed to router
- BREAKING CHANGE t type has been renamed to router
- BREAKING CHANGE a decoder has been renamed to a arg
- BREAKING CHANGE a uri has been renamed to a request_target
- FIX Fix matching query components in request target
- FIX Fix pretty printing of request_target
- NEW Add path, query and uri combinators to create and manipulate path, query and uri values
- NEW Add route, routes and pp_request_target functions
- NEW Add rest type and function rest_to_string
- CHANGE Refer splat as rest in the documentation.

## v2.0.0 2021-08-02 

- Convert assert based tests to ppx_expect tests
- Split README.md mdx test from tests
- BREAKING CHANGE ppx {%wtr| /home/about |} now produces a function
  expecting a handler rather than a uri. Demos and tests have been updated to
  reflect this change.
- BREAKING CHANGE remove functions pp_uri and >-.
- BREAKING CHANGE create function now mandates [route list list] to create
  a router. Hopefully this should not cause much breakage since the %wtr ppx 
  produces this value.
- NEW  %wtr ppx now allows specifying HTTP methods along with the uri route. A new 
  type meth has been added to represent all standard HTTP methods.
- NEW  Add Wtr.pp_route, Wtr.pp_method and Wtr.pp to pretty print a route, method 
  and a router respectively.

## v1.0.0 2021-05-11 UK

- First release.
