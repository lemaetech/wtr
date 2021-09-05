(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *-------------------------------------------------------------------------*)

(** A HTTP request routing library.

    [Wtr] ({i Well Typed Router}) is a HTTP request routing library for OCaml
    web applications.

    Given a HTTP {i request_target} and a HTTP {i method}, [Wtr] attempts to
    match the two properties to a pre-defined set of {!type:route}s. If a match
    is found then the corresponding {i route handler} function of the matched
    route is executed.

    The route matching algorithm is {i radix trie}.

    The {i well typed} part in [Wtr] means that the {i route handler} functions
    can capture and receive arguments which are typed in a variety of OCaml
    types.

    There are two ways to specify {i route}s:

    - Combinators based approach - {{!section:request_target_dsl} Request Target
      DSL}
    - Ppx based approach. The ppx [\[%routes "" \]] is provided by a separate
      opam package [wtr-ppx].

    {3 References}

    - {{:https://datatracker.ietf.org/doc/html/rfc7230#section-5.3} RFC 7230 -
      HTTP Request Target}
    - {{:https://datatracker.ietf.org/doc/html/rfc7231#section-4} RFC 7231 -
      HTTP Methods} *)

(** {1 Types} *)

(** A {!type:router} consists of one or many HTTP request {!type:route}s which
    are used to match a given HTTP request target.

    ['a] represents the value returned after executing the corresponding route
    handler of a matched route. *)
type 'a router

(** {!type:route} is a HTTP request route. A route encapsulates a HTTP
    {!type:method'}, a {!type:request_target} and a {i route handler}. A
    {i route handler} is either of the following:

    - a value of type ['a]
    - or a function which returns a value of type ['a]. *)
and 'a route

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
and ('a, 'b) request_target

(** {!type:path} is a part of {!type:request_target}. It consists of one or more
    {b path component}s. {b path component}s are tokens which are delimited by a
    [/] character token.

    Example of {i path} and {i path component}s:

    - [/] has path a component [/]
    - [/home/about] has path components [home, about]
    - [/home/contact/] has path components [home], [contact] and [/]

    Consult {{!section:request_target_dsl} Request Target DSL} for creating
    values of this type. *)
and ('a, 'b) path

(** {!type:query} is a part of {!type:request_target}. It consists of one of
    more {b query component}s which are delimited by a [&] character token. A
    {b query component} further consists of a pair of values called [name] and
    [value]. [name] and [value] tokens are delimited by a [=] character token. A
    {b query component} is represented syntactically as [(name,value)].

    Given a {i request_target} [/home/about?a=2&b=3], the {b query component}s
    are [(a,2)] and [(b,3)].

    Consult {{!section:request_target_dsl} Request Target DSL} for creating
    values of this type. *)
and ('a, 'b) query

(** {!type:method'} is a HTTP request method. See
    {{:https://datatracker.ietf.org/doc/html/rfc7231#section-4} HTTP RFC 7231 -
    HTTP Methods} *)
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

(** {!type:arg} is a component which can convert a {b path component} or a
    {b query component} [value] token into an OCaml typed value represented by
    ['a]. The successfully converted value is then fed to a {i route handler}
    function as an argument. *)
and 'a arg

(** {1:arg_func Arg} *)

val arg : string -> (string -> 'a option) -> 'a arg
(** [arg name convert] is {!type:arg} with name [name] and [convert] as the
    function which will convert/decode a string value to an OCaml value of type
    ['a].

    [name] is used during the pretty-printing of {i request_target} by
    {!val:pp_request_target}.

    [convert v] is [Some a] if [convert] can successfully convert [v] to [a].
    Otherwise it is [None].

    The following defines an arg of type [Fruit.t arg].

    {[
      module Fruit = struct
        type t = Apple | Orange | Pineapple

        let t : t Wtr.arg =
          Wtr.arg "fruit" (function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None )
      end
    ]} *)

(** {1:request_target_dsl Request Target DSL}

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

    {4 Illustration 1: request_target consisting of path and query}

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
    which encapsulates {!type:path} value and [q] is a closure which encapsulate
    {!type:query} value. *)

val ( /. ) :
  (('d, 'e) path -> ('b, 'c) path) -> ('d, 'e) path -> ('b, 'c) request_target
(** [ p /. pe] is a {!type:request_target} value that consists of only path
    components. [pe] is a path value that match the last path component. *)

val ( /?. ) :
  (('b, 'b) query -> ('c, 'd) path) -> unit -> ('c, 'd) request_target
(** [ pq /?. ()] is a {!type:request_target} value. [pq] is a closure which
    encapulates both {!type:path} and {!type:query} components.

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

    {3 Path} *)

val int : ('a, 'b) path -> (int -> 'a, 'b) path
(** [int] matches valid OCaml [int] values. *)

val int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path
(** [int32] matches valid OCaml [int32] values. *)

val int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path
(** [int64] matches valid OCaml [int64] values. *)

val float : ('a, 'b) path -> (float -> 'a, 'b) path
(** [float] matches valid OCaml [float] values. *)

val bool : ('a, 'b) path -> (bool -> 'a, 'b) path
(** [bool] matches a path component if it is equal to either ["true"] or
    ["false"] and converts them to valid OCaml [bool] values. *)

val string : ('a, 'b) path -> (string -> 'a, 'b) path
(** [string] matches valid OCaml [string] values. *)

val parg : 'c arg -> ('a, 'b) path -> ('c -> 'a, 'b) path
(** [parg d p] matches a path component if arg [d] can successfully convert path
    component to a value of type ['c]. *)

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
    token of {i query component}. *)

val qbool : string -> ('a, 'b) query -> (bool -> 'a, 'b) query
(** [qbool field] matches query component if [value] token is equal to either
    ["true"] or ["false"]. The [value] token is then converted to a valid OCaml
    [bool] value. [field] is the [name] token of {i query component}. *)

val qstring : string -> ('a, 'b) query -> (string -> 'a, 'b) query
(** [qstring field] matches a valid OCaml [string] value. [field] is the [name]
    token of {i query component}. *)

val qarg : string * 'c arg -> ('a, 'b) query -> ('c -> 'a, 'b) query
(** [qarg (field, d)] matches a query component if {i arg} [d] can successfully
    convert path component to a value of type ['c]. *)

(** {2 Matching Last Components}

    These combinators match the last(end) path component. They are used with
    {!val:(/.)} function. *)

val pend : ('a, 'a) path
(** [pend] matches the end of {!type:path} value. *)

val splat : (string -> 'a, 'a) path
(** [splat] matches and captures all of the remaining path and query components.
    The captured value is then fed to a {i route handler}. *)

val slash : ('a, 'a) path
(** [slash] matches path component [/] first and then matches the end of the
    {!type:path} value. *)

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

(** {1 Route and Router} *)

val route : ?method':method' -> ('a, 'b) request_target -> 'a -> 'b route
(** [route ~method' request_target handler] is a {!type:route}. The default
    value for [?method] is [`GET]. *)

val routes : method' list -> ('a, 'b) request_target -> 'a -> 'b route list
(** [routes methods request_target handler] is a list of routes in which all
    have the same [request_target] and route [handler] value but each have one
    [method'] from [methods]. This is equivalent to calling {!val:route} like
    so:

    {[
      List.map (fun m -> route ~method:m request_target handler) [meth1; meth2; meth3]
    ]} *)

val router : 'a route list -> 'a router
(** [router routes] is a {!type:router} made up of given [routes]. *)

val router' : 'a route list list -> 'a router
(** [router' routes_list = router (List.concat routes_list)] *)

val match' : method' -> string -> 'a router -> 'a option
(** [match' method' request_target router] is [Some a] if [method'] and
    [request_target] together matches one of the routes defined in [router].
    Otherwise it is None. *)

(** {1:pp Pretty Printers} *)

val pp_request_target : Format.formatter -> ('a, 'b) request_target -> unit
val pp_method : Format.formatter -> method' -> unit
val pp_route : Format.formatter -> 'b route -> unit
val pp : Format.formatter -> 'a router -> unit

(**/**)

(** Used by wtr/request_target ppx *)
module Private : sig
  val nil : ('b, 'b) request_target
  val splat : (string -> 'b, 'b) request_target
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
