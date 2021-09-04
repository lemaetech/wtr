(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *-------------------------------------------------------------------------*)

(** {1 Types} *)

(** A {!type:router} consists of one or many HTTP request {!type:route}s. These
    routes are used to match a given HTTP request target using a radix trie
    algorithm.

    ['a] represents the value returned after executing the corresponding route
    handler of a matched route. *)
type 'a router

(** {!type:route} is a HTTP request route. A route encapsulates a HTTP
    {!type:method'}, a {!type:uri} and a {i route handler}. A {i route handler}
    is either of the following:

    - a value of type ['a]
    - a function which returns values of type ['a]. *)
and 'a route

(** {!type:uri} represents a HTTP uri value. It encapsulates either just a
    {!type:path} value or a combination of {!type:path} and {!type:query}
    values.

    Example uri values:

    - [/home/about/] - path only uri
    - [/home/contact] - path only uri
    - [/home/contact?name=a&no=123] - path ([/home/contact]) and query
      ([name=a&no=123]) uri. Path and query are delimited by [?] character token
      if both are specified.

    Consult {{!section:uri} uri combinators} for creating values of this type. *)
and ('a, 'b) uri

(** {!type:path} is a part of {!type:uri}. It consists of one or more
    {b path component}s. {b path component}s are tokens which are delimited by a
    [/] character token.

    Example of {i path} and {i path component}s:

    - [/] has path a component [/]
    - [/home/about] has path components [home, about]
    - [/home/contact/] has path components [home, contact]

    Consult {{!section:path} path combinators} for creating values of this type. *)
and ('a, 'b) path

(** {!type:query} is a part of {!type:uri}. It consists of one of more
    {b query component}s which are delimited by a [&] character token. A
    {b query component} further consists of a pair of values called [name] and
    [value]. [name] and [value] tokens are delimited by a [=] character token. A
    {b query component} is represented syntactically as [(name,value)].

    Given a uri [/home/about?a=2&b=3], the {b query component}s are [(a,2)] and
    [(b,3)].

    Consult {{!section:query} query combinators} for creating values of this
    type. *)
and ('a, 'b) query

(** {!type:method'} is a HTTP request method. *)
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
    ['a]. The successfully converted value is then fed to a {i route} handler
    function as an argument. *)
and 'a arg

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

(** {1:arg_func Arg} *)

val arg : string -> (string -> 'a option) -> 'a arg
(** [arg name convert] is {!type:arg} with a name [name] and [convert] as the
    function which will convert/decode a string value to an OCaml value of type
    ['a].

    [name] is used during the pretty-printing of uri by {!val:pp_uri}. It is
    printed by being prefixed by [:], e.g. [:int], [:int32], [:string] etc.

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

(** {1:path Specifying Path Components}

    Path combinators are used to specify {b path component}s and {!type:path}
    values and hence {!type:uri} values.

    Let's assume that we want to specify a HTTP route which matches a request
    path as such:

    + match a string literal "hello" exactly
    + followed by a valid OCaml [int] value
    + and then followed by an OCaml [string] value

    We can use path combinators to implement such a requirement:

    {[ let uri = Wtr.(exact "hello" / int / string /. pend) ]}

    The [uri] value above matches the following HTTP request targets:

    - [/home/2/str1]
    - [/home/3/str2]
    - [/home/-10/str3] *)

val exact : string -> ('a, 'b) path -> ('a, 'b) path
(** [exact e p] matches path component [p1] to [e] exactly. *)

(** {3:path-decoder Decoders} *)

val int : ('a, 'b) path -> (int -> 'a, 'b) path
(** [int] matches and captures a valid OCaml [int] value. *)

val int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path
(** [int32] matches and captures a valid OCaml [int32] values. *)

val int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path
(** [int64] matches and captures a valid OCaml [int64] values. *)

val float : ('a, 'b) path -> (float -> 'a, 'b) path
(** [float] matches and captures a valid OCaml [float] values. *)

val bool : ('a, 'b) path -> (bool -> 'a, 'b) path
(** [bool] matches and captures a valid OCaml [bool] values. *)

val string : ('a, 'b) path -> (string -> 'a, 'b) path
(** [string] matches and captures a valid OCaml [string] values. *)

val decode : 'c arg -> ('a, 'b) path -> ('c -> 'a, 'b) path
(** [decode d p] matches a path component if decoder [d] can successfully decode
    to [Some c]. *)

(** {3 End Path components}

    These combinators match the last(end) path components. They are used with
    {!val:(/.)} function. *)

val pend : ('a, 'a) path
(** [pend] matches the end of {!type:path} value. *)

val splat : (string -> 'a, 'a) path
(** [splat] matches and captures all of the remaining path and query components. *)

val slash : ('a, 'a) path
(** [slash] matches path component [/] and the end of the {!type:path} value. *)

(** {3 Create URI from Paths} *)

val ( / ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) path) -> 'd -> 'c
(** [ p1 / p2] is a closure that encapsulates closures [p1] and [p2] which both
    encapsulates {!type:path} values. *)

val ( /. ) : (('d, 'e) path -> ('b, 'c) path) -> ('d, 'e) path -> ('b, 'c) uri
(** [/.] is a {!type:uri} value that consists of only path components. *)

val to_uri : ('a, 'b) path -> ('a, 'b) uri
(** [to_uri p] is {!type:uri} consisting of only path [p]. *)

(** {1:query Query} *)

val qint : string -> ('a, 'b) query -> (int -> 'a, 'b) query
val qint32 : string -> ('a, 'b) query -> (int32 -> 'a, 'b) query
val qint64 : string -> ('a, 'b) query -> (int64 -> 'a, 'b) query
val qfloat : string -> ('a, 'b) query -> (float -> 'a, 'b) query
val qbool : string -> ('a, 'b) query -> (bool -> 'a, 'b) query
val qstring : string -> ('a, 'b) query -> (string -> 'a, 'b) query
val qdecode : string * 'c arg -> ('a, 'b) query -> ('c -> 'a, 'b) query
val qexact : string * string -> ('a, 'b) query -> ('a, 'b) query
val ( /& ) : (('a, 'b) query -> 'c) -> ('d -> ('a, 'b) query) -> 'd -> 'c

(** {1:uri URI} *)

val root : ('a, 'a) uri
(** [root] is [/] request uri, i.e. it matches the exactly the root HTTP
    request. *)

val ( /? ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) query) -> 'd -> 'c
(** [ pc /? qc] is a closure which encapsulates path closure [pc] and query
    closure [qc]. *)

val ( /?. ) : (('b, 'b) query -> ('c, 'd) path) -> unit -> ('c, 'd) uri
(** [ pqc /?. ()] is a {!type:uri} where [pqc] is a closure encapulating both
    {!type:path} and {!type:query} - see {!val:(/?)}.

    {[
      let uri1 =
        exact "hello" / bool /? qint "hello" /& qstring "hh" /& qbool "b" /?. ()
    ]} *)

(** {1 Route and Router} *)

val route : ?method':method' -> ('a, 'b) uri -> 'a -> 'b route
(** [route ~method' uri handler] is a {!type:route}. The default value for
    [?method] is [`GET]. *)

val routes : method' list -> ('a, 'b) uri -> 'a -> 'b route list
(** [routes methods uri handler] is a list of routes in which all have the same
    [uri] and route [handler] value but each have one [method'] from [methods].
    This is equivalent to calling {!val:route} like so:

    {[ List.map (fun m -> route ~method:m uri handler) [meth1; meth2; meth3] ]} *)

val router : 'a route list -> 'a router
(** [router routes] is a {!type:router} made up of given [routes]. *)

val router' : 'a route list list -> 'a router
(** [router' routes_list = router (List.concat routes_list)] *)

val match' : method' -> string -> 'a router -> 'a option
(** [match' method' request_target router] is [Some a] if [method'] and
    [request_target] together matches one of the routes defined in [router].
    Otherwise it is None. *)

(** {1:pp Pretty Printers} *)

val pp_method : Format.formatter -> method' -> unit
val pp_route : Format.formatter -> 'b route -> unit
val pp_uri : Format.formatter -> ('a, 'b) uri -> unit
val pp : Format.formatter -> 'a router -> unit

(**/**)

(** Used by wtr/uri ppx *)
module Private : sig
  val nil : ('b, 'b) uri
  val splat : (string -> 'b, 'b) uri
  val slash : ('b, 'b) uri
  val exact : string -> ('a, 'b) uri -> ('a, 'b) uri
  val query_exact : string -> string -> ('a, 'b) uri -> ('a, 'b) uri
  val decode : 'c arg -> ('a, 'b) uri -> ('c -> 'a, 'b) uri
  val query_decode : string -> 'c arg -> ('a, 'b) uri -> ('c -> 'a, 'b) uri
  val int : int arg
  val int32 : int32 arg
  val int64 : int64 arg
  val float : float arg
  val bool : bool arg
  val string : string arg
end

(**/**)
