(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *-------------------------------------------------------------------------*)

(** {i Well Typed Router} - is a HTTP request routing library for OCaml web
    applications.

    Given a HTTP {i request_target} and a HTTP {i method}, [Wtr] attempts to
    match the two properties to a pre-defined set of {i route}s. If a match is
    found then the corresponding {i route handler} function of the matched route
    is executed.

    The route matching algorithm is {i radix trie}.

    The {i well typed} part in [Wtr] means that the {i route handler} functions
    can capture and receive arguments which are typed in a variety of OCaml
    types.

    There are two ways to specify {i route} and {i request target}s:

    - {{!section:request_target_dsl} Request Target Combinators} - combinators
      based
    - [\[%routes ""\]] - ppx based which is provided by a separate opam package
      [wtr-ppx]. *)

(** {1 Types} *)

type 'a router
(** A {!type:router} consists of one or many HTTP request {!type:route}s which
    are used to match a given HTTP request target.

    ['a] is a value which is returned by a {i route handler} of the matched
    {i route}. *)

and 'a route
(** {!type:route} is a HTTP request route. A route encapsulates a HTTP
    {!type:method'}, a {!type:request_target} and a {i route handler}. A
    {i route handler} is either of the following:

    - a value of type ['a]
    - or a function which returns a value of type ['a]. *)

and 'a routes = 'a route list

and ('a, 'b) request_target
(** {!type:request_target} is a HTTP request target value to be matched. It
    consists of either just a {!type:path} value or a combination of
    {!type:path} and {!type:query} values.

    Example {i request_target} values:

    - [/home/about/] - path only
    - [/home/contact] - path only
    - [/home/contact?name=a&no=123] - path ([/home/contact]) and query
      ([name=a&no=123]). Path and query are delimited by [?] character token if
      both are specified.

    Consult {{!section:request_target_dsl} Request Target DSL} for creating
    values of this type.

    See {{:https://datatracker.ietf.org/doc/html/rfc7230#section-5.3} HTTP RFC
    7230 - request target}. *)

and ('a, 'b) path
(** {!type:path} is a part of {!type:request_target}. It consists of one or more
    {b path component}s. {b path component}s are tokens which are delimited by a
    [/] character token.

    Example of {i path} and {i path component}s:

    - [/] has path a component [/]
    - [/home/about] has path components [home, about]
    - [/home/contact/] has path components [home], [contact] and [/]

    Consult {{!section:request_target_dsl} Request Target DSL} for creating
    values of this type. *)

and rest
(** {!type:rest} represents a part of {i request target} from a given path
    component to the rest of a {i request_target}.

    Use {!val:rest_to_string} to convert to string representation. *)

and ('a, 'b) query
(** {!type:query} is a part of {!type:request_target}. It consists of one of
    more {b query component}s which are delimited by a [&] character token. A
    {b query component} further consists of a pair of values called [name] and
    [value]. [name] and [value] tokens are delimited by a [=] character token. A
    {b query component} is represented syntactically as [(name,value)].

    Given a {i request_target} [/home/about?a=2&b=3], the {b query component}s
    are [(a,2)] and [(b,3)].

    Consult {{!section:request_target_dsl} Request Target DSL} for creating
    values of this type. *)

and method' =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]
(** {!type:method'} is a HTTP request method. See
    {{:https://datatracker.ietf.org/doc/html/rfc7231#section-4} HTTP RFC 7231 -
    HTTP Methods} *)

and 'a arg
(** {!type:arg} is a component which can convert a {b path component} or a
    {b query component} [value] token into an OCaml typed value represented by
    ['a]. The successfully converted value is then fed to a {i route handler}
    function as an argument. *)

(** {1:arg_func Arg} *)

val arg : string -> (string -> 'a option) -> 'a arg
(** [arg name convert] is {!type:arg} with name [name] and [convert] as the
    function which will convert/decode a string value to an OCaml value of type
    ['a].

    [name] is used during the pretty-printing of {i request_target} by
    {!val:pp_request_target}.

    [convert v] is [Some a] if [convert] can successfully convert [v] to [a].
    Otherwise it is [None].

    Although not strictly necessary if we are only working with
    {i Request Target DSL}, it is recommended to adhere to the following
    convention when creating a custom arg. Such an ['a arg] value can be used
    with both {i Request Target DSL} and [wtr-ppx] ppxes. The convention is as
    follows:

    + Arg value be encapsulated in a module
    + The module define a type called [t]
    + The module define a value called [t] which is of type [t Wtr.arg]
    + The [name] value of the {i arg} match the name of the module.

    An example of such an ['a arg] component - [Fruit.t arg] is as below:

    {[
      module Fruit = struct
        type t = Apple | Orange | Pineapple

        let t : t Wtr.arg =
          Wtr.arg "Fruit" (function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None)
      end
    ]}

    See {!val:parg} and {!val:qarg} for usage in {i path} and {i query}
    components. *)

(** {1:request_target_dsl Request Target Combinators}

    Request Target combinators implement a DSL(domain specific language) to
    specify {!type:request_target}, {b path component} and {b query component}
    values.

    {4 Illustration 1: request_target consisting of path only}

    Let's assume that we want to specify a HTTP route which matches a request
    target value as such:

    + match a string literal "home" exactly
    + followed by a valid OCaml [int] value
    + and then finally followed by an OCaml [string] value

    The {i request target} is implemented as such:

    {[ let target1 = Wtr.(exact "hello" / int / string /. pend) ]}

    [target1] above matches the following instances of HTTP request target:

    - [/home/2/str1]
    - [/home/-10/str3]

    {4 Illustration 2: request_target consisting of path and query}

    Let's assume that we want to specify a HTTP route which matches a request
    target value which consists of both path and query as such:

    + match a string literal "hello" exactly
    + followed by a valid OCaml [bool] value
    + followed by a query component where the field name is "i" and the query
      value is a valid OCaml [int] value.
    + and then finally a query component where the field name is "s" and the
      query value is an OCaml [string] value.

    The {i request_target} is implemented as such:

    {[
      let target2 = Wtr.(exact "hello" / bool /? qint "i" / qstring "s" /?. ())
    ]}

    [target2] above matches the following instances of HTTP request target:

    - [/hello/true?i=233&s=str1]
    - [/hello/false?i=-1234&s=str2] *)

(** {2 General Components} *)

val ( / ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) path) -> 'd -> 'c
(** [ p1 / p2] is a closure that {i combines} [p1] and [p2]. [p1] and [p2] are
    closures which encapsulate {!type:path} value. *)

val ( /& ) : (('a, 'b) query -> 'c) -> ('d -> ('a, 'b) query) -> 'd -> 'c
(** [q1 /& q1] is a closure that {i combines} [q1] and [q2]. [q1] and [q2] are
    closures which encapsulate {!type:query} value. *)

val ( /? ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) query) -> 'd -> 'c
(** [ p /? q] is a closure which {i combines} [p] and [q]. [p] is a closure
    which encapsulates {!type:path} value and [q] is a closure which
    encapsulates {!type:query} value. *)

val ( //. ) : (('d, 'e) path -> ('b, 'c) path) -> ('d, 'e) path -> ('b, 'c) path
(** [ p //. pe] is {!type:path} that consists of only path components [p] and
    [pe]. [pe] is a path value that matches the last path component. *)

val ( /. ) :
  (('d, 'e) path -> ('b, 'c) path) -> ('d, 'e) path -> ('b, 'c) request_target
(** [ p /. pe] is a {!type:request_target} value that consists of only path
    components [p] and [pe]. [pe] is a path value that matches the last path
    component. It is equivalent to the following:

    {[ let p = Wtr.(exact "hello" / exact "about" //. pend) |> Wtr.of_path ]} *)

val ( /?. ) :
  (('b, 'b) query -> ('c, 'd) path) -> unit -> ('c, 'd) request_target
(** [ pq /?. ()] is {!type:request_target}. [pq] is a closure which encapulates
    both {!type:path} and {!type:query} components.

    {[
      let request_target1 =
        Wtr.(
          exact "hello"
          / bool
          /? qint "hello"
          /& qstring "hh"
          /& qbool "b"
          /?. ())
    ]} *)

val of_path : ('a, 'b) path -> ('a, 'b) request_target
(** [of_path path] converts [path] to {!type:request_target} *)

val exact : string -> ('a, 'b) path -> ('a, 'b) path
(** [exact e p] matches a path component to [e] exactly. *)

val qexact : string * string -> ('a, 'b) query -> ('a, 'b) query
(** [qexact (field, e)] matches a query component to [e] exactly. The query
    component token [name] value is [field]. *)

val to_request_target : ('a, 'b) path -> ('a, 'b) request_target
(** [to_request_target p] is {!type:request_target} consisting of only path [p]. *)

val root : ('a, 'a) request_target
(** [root] is a {i request_target} with [/] as the only component, i.e. it
    matches exactly the root HTTP request. *)

(** {2 Arg Components}

    Path/Query arg components encapsulate {!type:arg} value which are then fed
    to a {i route handler} function as an argument.

    {3:path_arg Path} *)

val int : ('a, 'b) path -> (int -> 'a, 'b) path
(** [int] matches valid OCaml [int] values. *)

val int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path
(** [int32] matches valid OCaml [int32] values. *)

val int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path
(** [int64] matches valid OCaml [int64] values. *)

val float : ('a, 'b) path -> (float -> 'a, 'b) path
(** [float] matches valid OCaml [float] and [int] values.

    {b Note} In addition to OCaml [float] values, the combinator can also match
    OCaml [int] values. Therefore:

    Given, [p] is

    {[ let p = Wtr.(float /. pend) ]}

    then, it can match the following instances of HTTP request targets:

    - [/123]
    - [/-234]
    - [/123.]
    - [/123.02]
    - [/-123.]
    - [/-123.22] *)

val bool : ('a, 'b) path -> (bool -> 'a, 'b) path
(** [bool] matches a path component if it is equal to either ["true"] or
    ["false"] and converts them to valid OCaml [bool] values. *)

val string : ('a, 'b) path -> (string -> 'a, 'b) path
(** [string] matches valid OCaml [string] values. *)

val parg : 'c arg -> ('a, 'b) path -> ('c -> 'a, 'b) path
(** [parg d p] matches a path component if [d] can successfully convert path
    component to a value of type ['c].

    The example below uses the [Fruit.t arg] defined {{!section:arg_func}
    above}:

    {[ let p = Wtr.(parg Fruit.t /. pend) ]}

    [p] matchs the following instances of HTTP request target values:

    - [/pineapple]
    - [/apple]
    - [/orange] *)

(** {3 Query} *)

val qint : string -> ('a, 'b) query -> (int -> 'a, 'b) query
(** [qint field] matches a valid OCaml [int] value. [field] is the [name] token
    of {i query component}. *)

val qint32 : string -> ('a, 'b) query -> (int32 -> 'a, 'b) query
(** [qint32 field] matches a valid OCaml [int32] value. [field] is the [name]
    token of {i query component}. *)

val qint64 : string -> ('a, 'b) query -> (int64 -> 'a, 'b) query
(** [qint64 field] matches a valid OCaml [int64] value. [field] is the [name]
    token of {i query component}. *)

val qfloat : string -> ('a, 'b) query -> (float -> 'a, 'b) query
(** [qfloat field] matches a valid OCaml [float] value. [field] is the [name]
    token of {i query component}.

    The values matched by this combinator is the same as the {!val:float}
    combinator. *)

val qbool : string -> ('a, 'b) query -> (bool -> 'a, 'b) query
(** [qbool field] matches query component if [value] token is equal to either
    ["true"] or ["false"]. The [value] token is then converted to a valid OCaml
    [bool] value. [field] is the [name] token of {i query component}. *)

val qstring : string -> ('a, 'b) query -> (string -> 'a, 'b) query
(** [qstring field] matches a valid OCaml [string] value. [field] is the [name]
    token of {i query component}. *)

val qarg : string * 'c arg -> ('a, 'b) query -> ('c -> 'a, 'b) query
(** [qarg (field, d)] matches a query component if [d] can successfully convert
    path component to a value of type ['c]. [field] is the [name] token of
    {i query component}.

    The example below uses the [Fruit.t arg] defined {{!section:arg_func}
    above}:

    {[ let p = Wtr.(exact "hello" /? qarg ("fruit", Fruit.t) /?. ()) ]}

    [p] matchs the following instances of HTTP request target values:

    - [/hello?fruit=pineapple]
    - [/hello?fruit=apple]
    - [/hello?fruit=orange] *)

(** {2 Last Path Component}

    These combinators match the last - {i end} - path component. They are used
    with {!val:(/.)} function. *)

val pend : ('a, 'a) path
(** [pend] matches the end of {!type:path} value. *)

val rest : (rest -> 'a, 'a) path
(** [rest] matches and captures all of the remaining path and query components.
    The captured value is then fed to a {i route handler}.

    {[
      let%expect_test "rest: comb" =
        (Wtr.(
           router [ routes [ `GET ] (exact "public" /. rest) rest_to_string ])
         |> Wtr.match' `GET "/public/styles/style.css"
         |> function
         | Some s -> print_string s
         | None -> ());
        [%expect {| styles/style.css |}]
    ]} *)

val slash : ('a, 'a) path
(** [slash] matches path component [/] first and then matches the end of the
    {!type:path} value.

    {[
      let%expect_test "slash matched" =
        (Wtr.(router [ routes [ `GET ] (exact "public" /. slash) "slash" ])
         |> Wtr.match' `GET "/public/"
         |> function
         | Some s -> print_string s
         | None -> ());
        [%expect {| slash |}]

      let%expect_test "slash not matched" =
        (Wtr.(router [ routes [ `GET ] (exact "public" /. slash) "slash" ])
         |> Wtr.match' `GET "/public"
         |> function
         | Some s -> print_string s
         | None -> ());
        [%expect {| |}]
    ]} *)

val rest_to_string : rest -> string
(** [rest_to_string rest] converts [rest] to string. *)

(** {1 Routes and Router} *)

val route : method' -> ('a, 'b) request_target -> 'a -> 'b route
(** [route method' request_target handler] is a {!type:route}. *)

val routes : method' list -> ('a, 'b) request_target -> 'a -> 'b routes
(** [routes methods request_target route_handler] is a product of
    [methods X request_target X route_handler]. This is equivalent to calling
    {!val:route} like so:

    {[
      List.map (fun m -> route ~method:m request_target route_handler) [meth1; meth2; meth3]
    ]} *)

val router : 'a routes list -> 'a router
(** [router routes] is a {!type:router} that is composed of [routes]. *)

val match' : method' -> string -> 'a router -> 'a option
(** [match' method' request_target router] is [Some a] if [method'] and
    [request_target] together matches one of the routes defined in [router].
    Otherwise it is None. The value [Some a] is returned by the
    {i route handler} of the matched {i route}.

    The routes are matched based on the lexical order of the routes. This means
    they are matched from {i top to bottom}, {i left to right} and to the
    {i longest match}. See {!val:pp} to visualize the router and the route
    matching mechanism. *)

(** {1 HTTP Method} *)

val method_equal : method' -> method' -> bool
(** [method_equal m1 m2] is [true] if [m1] and [m2] is the same value. Otherwise
    it is [false].

    {i Note} if both [m1] and [m2] are [`Method m] then the string comparison is
    case insensitive.

    {[
      Wtr.method_equal `GET `GET = true;;
      Wtr.method_equal `POST `GET = false;;
      Wtr.method_equal (`Method "meth") (`Method "METH") = true
    ]} *)

val method' : string -> method'
(** [method' m] is {!type:method'} where string value [m] is converted to
    {!type:method'} as follows:

    - ["GET"] to [`GET]
    - ["HEAD"] to [`HEAD]
    - ["POST"] to [`POST]
    - ["PUT"] to [`PUT]
    - ["DELETE"] to [`DELETE]
    - ["CONNECT"] to [`CONNECT]
    - ["OPTIONS"] to [`OPTIONS]
    - ["TRACE"] to [`TRACE]
    - Any other value [m] to [`Method m]

    {i Note} String comparison is case insensitive.

    {[
      Wtr.method' "GET" = `GET;;
      Wtr.method' "get" = `GET;;
      Wtr.method' "method" = `Method "method"
    ]} *)

(** {1:pp Pretty Printers and Debugging}

    Pretty printers can be useful during debugging of routing and/or route
    related issues. *)

val pp_request_target : Format.formatter -> ('a, 'b) request_target -> unit
(** [pp_request_target fmt target] pretty prints [target] onto [fmt].

    {b Path components}

    {i arg components} - name of the combinator prefixed by [:] token, eg.
    {!val:int} is printed as [:int], {!val:float} is printed as [:float].
    {{!section:path_arg} Path Arg Components}

    {i {!val:parg} component} - name of the arg followed by [:] token e.g.
    [parg Fruit.t] is printed as [:Fruit].

    {i {!val:exact} component} - the string literal given to [exact] is printed,
    e.g. [exact "hello"] is printed as [hello].

    {i {!val:slash}} - printed as [/]

    {i {!val:rest}} - printed as [**]

    A [/] character is inserted in between the components when printing a
    sequence of {i path components}, e.g.

    {[ let p = Wtr.(exact "hello" / int / bool /. rest) ]}

    is printed as [/hello/:int/:bool/**].

    {b Query components}

    {i arg components} - query arg components are printed similar to
    {i path arg components}; with the addition of [name] token, e.g. [qint "h"]
    is printed as [h=:int], [qbool "b"] is printed as [b=:bool].

    {i qarg component} - is printed similar to {!val:parg}; with the addition of
    field name, e.g. [qarg ("h", Fruit.t)] is printed as [h=:fruit].

    {i qexact component} - is printed similar to {!val:exact}; with the addition
    of field name, e.g. [qexact ("h", "hello")] is printed as [h=hello].

    A [&] character is inserted in between the components when printing a
    sequence of {i query components}. Additionally, a [?] character is printed
    in between {i path components} and {i query components}, e.g.

    {[
      let target1 =
        Wtr.(
          exact "hello"
          / bool
          / int
          / string
          /? qexact ("h", "hello")
          /& qbool "b"
          /?. ())
      in
      Wtr.pp_request_target Format.std_formatter target1
    ]}

    will print the following: [/hello/:bool/:int/:string?h=hello&b=:bool] *)

val pp_method : Format.formatter -> method' -> unit
(** [pp_method fmt m] pretty prints [m] onto [fmt]. It does the inverse of
    {!val:method'}. *)

val pp_route : Format.formatter -> 'b route -> unit
(** [pp_route fmt route] first pretty prints the [method] followed by the
    [request_target] of a [route], e.g.

    {[
      let route1 =
        Wtr.(route ~method':`GET (exact "hello" / bool /. slash)) (fun _ -> ())
    ]}

    [route1] is pretty printed as [GET/hello/:bool/]

    The [route2] contains both path and query components:

    {[
      let route2 =
        Wtr.(
          route ~method':`GET
            (exact "hello" / bool /? qexact ("h", "hello") /& qbool "b" /?. ()))
          (fun _ _ -> ())
      in
      Wtr.pp_route Format.std_formatter route2
    ]}

    It is printed as follows: [GET/hello/:bool?h=hello?b=:bool] *)

val pp : Format.formatter -> 'a router -> unit
(** [pp fmt router] pretty prints [router] onto [fmt]. It follows the same
    mechanism as {!val:pp_route} and {!val:pp_request_target}. However, unlike
    the two functions, it prints each component - {i path} and {i query} - onto
    a separate line. The component in each line is indented.

    The indentation and line printing is meant to convey the order of a route
    component evaluation. The evaluation is from top to bottom and left to
    right. This gives some indication of how the routes are evaluated and thus
    can be used to aid in debugging routing issues.

    For example, [router1] which is defined as:

    {[
      let router1 =
        Wtr.(
          router'
            [
              routes
                [ `GET; `POST; `HEAD; `DELETE ]
                (exact "home" / exact "about" /. slash)
                about_page;
              routes [ `GET ]
                (exact "contact" / string / int /. pend)
                contact_page;
              routes [ `GET ]
                (exact "product" / string /? qint "section" /& qbool "q" /?. ())
                product1;
              routes [ `GET ]
                (exact "product"
                / string
                /? qint "section"
                /& qexact ("q1", "yes")
                /?. ())
                product2;
              routes [ `GET ] (exact "fruit" / parg Fruit.t /. pend) fruit_page;
            ])
    ]}

    is pretty printed as below:

    {v
GET
  /home
    /about
      /
  /contact
    /:string
      /:int
  /product
    /:string
      ?section=:int
        &q=:bool
        &q1=yes
  /fruit
    /:Fruit
POST
  /home
    /about
      /
HEAD
  /home
    /about
      /
DELETE
  /home
    /about
      /
    v} *)

(** {1 References}

    - {{:https://datatracker.ietf.org/doc/html/rfc7230#section-5.3} RFC 7230 -
      HTTP Request Target}
    - {{:https://datatracker.ietf.org/doc/html/rfc7231#section-4} RFC 7231 -
      HTTP Methods}*)

(**/**)

(** Used by wtr/request_target ppx *)
module Private : sig
  val nil : ('b, 'b) request_target
  val rest : (rest -> 'b, 'b) request_target
  val slash : ('b, 'b) request_target
  val exact : string -> ('a, 'b) request_target -> ('a, 'b) request_target

  val query_exact :
    string -> string -> ('a, 'b) request_target -> ('a, 'b) request_target

  val arg : 'c arg -> ('a, 'b) request_target -> ('c -> 'a, 'b) request_target

  val query_arg :
    string -> 'c arg -> ('a, 'b) request_target -> ('c -> 'a, 'b) request_target

  val int : int arg
  val int32 : int32 arg
  val int64 : int64 arg
  val float : float arg
  val bool : bool arg
  val string : string arg
end

(**/**)
