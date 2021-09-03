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

(** {!type:uri} represents a HTTP uri value. It encapsulates both {!type:path}
    and {!type:query} in a route. Path and query are delimited by a [?]
    character token.

    Representative examples:

    - [/home/about/]
    - [/home/contact],
    - [/home/contact?name=a&no=123].

    Consult {{!section:uri} uri combinators} for creating values of this type. *)
and ('a, 'b) uri

(** {!type:path} is a part of {!type:uri} where the components are delimited by
    a [/] character token.

    Representative examples: [/], [/home/about], [/home/contact/].

    Consult {{!section:path} path combinators} for creating values of this type. *)
and ('a, 'b) path

(** {!type:query} is a part of {!type:uri}. Each query component is generally a
    pair of [name] and [value] - [(name,value)]. A query component in a uri is
    delimited by a [&] character token and the [name], [value] token is
    delimited by a [=] character token.

    Given a uri [/home/about?a=2&b=3], the query components are [(a,2)] and
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

(** {!type:decoder} is a uri component which can convert a HTTP request uri
    string value into an OCaml typed value (represented by ['a]). It can be used
    in both {!type:path} and {!type:query} values construction. *)
and 'a decoder

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

(** {1:decoder_func Decoder} *)

val decoder : string -> (string -> 'a option) -> 'a decoder
(** [decoder name decode] is {!type:decoder} with name [name] and [decode] as
    the string conversion/decoder function.

    [decode v] is [Some a] if [decode] can successfully convert [v] to [a].
    Otherwise it is [None].

    {[
      module Fruit = struct
        type t = Apple | Orange | Pineapple

        let t : t Wtr.decoder =
          Wtr.decoder "fruit" (function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None )
      end
    ]} *)

(** {1:path Path}

    Path combinators are used to create {!type:uri} and {!type:path} values.

    Given the [uri] below:

    {[ let uri = Wtr.(exact "hello" / int / string /. pend) ]}

    We can match the following HTTP request targets:

    - [/home/2/about]
    - [/home/3/contact] *)

(** {4 Decoders} *)

val int : ('a, 'b) path -> (int -> 'a, 'b) path
(** [int] is a path decoder that can decode [int] values. *)

val int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path
(** [int32] is a path decoder that can decode [int32] values. *)

val int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path
(** [int64] is a path decoder that can decode [int64] values. *)

val float : ('a, 'b) path -> (float -> 'a, 'b) path
(** [float] is a path decoder that can decode [float] values. *)

val bool : ('a, 'b) path -> (bool -> 'a, 'b) path
(** [bool] is a path decoder that can decode [bool] values. *)

val string : ('a, 'b) path -> (string -> 'a, 'b) path
(** [string] is a path decoder that can decode [string] values. *)

val decode : 'c decoder -> ('a, 'b) path -> ('c -> 'a, 'b) path
(** [decode d p] is a path component for custom decoder [d]. *)

val exact : string -> ('a, 'b) path -> ('a, 'b) path
(** [exact e p] is a path component that matches a uri token [e] exactly. *)

(** {4 Ending path construction}

    These combinators affect the last uri component. *)

val pend : ('a, 'a) path
(** [pend] ends path construction. *)

val splat : (string -> 'a, 'a) path
(** [splat] ends path construction by matching the remaining uri components. *)

val slash : ('a, 'a) path
(** [slash] end path construction by matching a trailing [/] value. *)

val ( / ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) path) -> 'd -> 'c
(** [ p1 / p2] is a closure that encapsulates closures [p1] and [p2] which both
    encapsulates {!type:path} values. *)

val ( /. ) : ('a -> ('b, 'c) path) -> 'a -> ('b, 'c) uri
(** [/.] is a {!type:uri} value that consists of only path components. *)

(** {1:query Query} *)

val qint : string -> ('a, 'b) query -> (int -> 'a, 'b) query
val qint32 : string -> ('a, 'b) query -> (int32 -> 'a, 'b) query
val qint64 : string -> ('a, 'b) query -> (int64 -> 'a, 'b) query
val qfloat : string -> ('a, 'b) query -> (float -> 'a, 'b) query
val qbool : string -> ('a, 'b) query -> (bool -> 'a, 'b) query
val qstring : string -> ('a, 'b) query -> (string -> 'a, 'b) query
val qdecode : string * 'c decoder -> ('a, 'b) query -> ('c -> 'a, 'b) query
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

val router : 'a route list list -> 'a router
(** [router routes] is a {!type:router} made up of given [routes]. *)

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
  val t_slash : ('b, 'b) uri
  val exact : string -> ('a, 'b) uri -> ('a, 'b) uri
  val query_exact : string -> string -> ('a, 'b) uri -> ('a, 'b) uri
  val decode : 'c decoder -> ('a, 'b) uri -> ('c -> 'a, 'b) uri
  val query_decode : string -> 'c decoder -> ('a, 'b) uri -> ('c -> 'a, 'b) uri
  val int : int decoder
  val int32 : int32 decoder
  val int64 : int64 decoder
  val float : float decoder
  val bool : bool decoder
  val string : string decoder
end

(**/**)
