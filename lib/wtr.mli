(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {1 Types} *)

(** A {!type:router} consists of one or many HTTP request {!type:route}s. These
    routes are used to match a given HTTP request target using a radix trie
    algorithm.

    ['a] represents the value returned after matching a given HTTP request
    target and executing the corresponding route handler. *)
type 'a router

(** {!type:route} is a HTTP request route. A route consists of a HTTP
    {!type:method'}, a {!type:uri} and a route handler. ['a] represents the
    value returned by the handler. It corresponds to ['a] in {!type:router}. *)
and 'a route

(** {!type:uri} represents a concatenation of {!type:path} and {!type:query} in
    a route, e.g. [/home/about/], [/home/contact],
    [/home/contact?name=a&no=123]. *)
and ('a, 'b) uri

(** {!type:path} is a HTTP uri path component, e.g.[/], [/home/about],
    [/home/contact/]. Consult {{!section:path} path combinators} for creating
    values of this type. *)
and ('a, 'b) path

(** {!type:query} is a HTTP uri query component. For a given HTTP uri
    [/home/about?a=2&b=3], all tokens after [?] is a {!type:query}, i.e.
    [a=2&b=3]. Consult {{!section:query} query combinators} for creating values
    of this type. *)
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

(** {!type:decoder} is a uri component which is responsible for decoding uri
    components - both path and query - from a string value to ['a]. *)
and 'a decoder

(** {1 HTTP Method} *)

val method_equal : method' -> method' -> bool
val method' : string -> method'

(** {1 Custom Decoder} *)

val decoder : name:string -> decode:(string -> 'a option) -> 'a decoder
(** [decoder ~name ~decode] creates a user defined custom uri decoder component.
    [name] is used during the pretty printing of [uri]. *)

(** {1 URI Combinators} *)

val end' : ('b, 'b) uri
val ( /? ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) query) -> 'd -> 'c
val ( /?. ) : (('a, 'b) query -> ('c, 'd) path) -> ('a, 'b) uri -> ('c, 'd) uri

(** {2:path Path} *)

val ( / ) : (('a, 'b) path -> 'c) -> ('d -> ('a, 'b) path) -> 'd -> 'c
val int : ('a, 'b) path -> (int -> 'a, 'b) path
val int32 : ('a, 'b) path -> (int32 -> 'a, 'b) path
val int64 : ('a, 'b) path -> (int64 -> 'a, 'b) path
val float : ('a, 'b) path -> (float -> 'a, 'b) path
val bool : ('a, 'b) path -> (bool -> 'a, 'b) path
val string : ('a, 'b) path -> (string -> 'a, 'b) path
val decode : 'c decoder -> ('a, 'b) path -> ('c -> 'a, 'b) path
val lit : string -> ('a, 'b) path -> ('a, 'b) path
val pend : ('b, 'b) path
val splat : (string -> 'b, 'b) path
val slash : ('b, 'b) path
val ( /. ) : ('a -> ('b, 'c) path) -> 'a -> ('b, 'c) uri

(** {2:query Query} *)

val ( /& ) : (('a, 'b) query -> 'c) -> ('d -> ('a, 'b) query) -> 'd -> 'c
val qint : string -> ('a, 'b) query -> (int -> 'a, 'b) query
val qint32 : string -> ('a, 'b) query -> (int32 -> 'a, 'b) query
val qint64 : string -> ('a, 'b) query -> (int64 -> 'a, 'b) query
val qfloat : string -> ('a, 'b) query -> (float -> 'a, 'b) query
val qbool : string -> ('a, 'b) query -> (bool -> 'a, 'b) query
val qstring : string -> ('a, 'b) query -> (string -> 'a, 'b) query
val qdecode : string * 'c decoder -> ('a, 'b) query -> ('c -> 'a, 'b) query
val qlit : string * string -> ('a, 'b) query -> ('a, 'b) query

(** {1 Route and Router} *)

val route : ?method':method' -> ('a, 'b) uri -> 'a -> 'b route
val routes : method' list -> ('a, 'b) uri -> 'a -> 'b route list

val router : 'a route list list -> 'a router
(** [route routes] is a router made up of given [routes]. It is used with [%wtr]
    ppx.

    {[ Wtr.router [%wtr "/public/**"] ]} *)

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
  val lit : string -> ('a, 'b) uri -> ('a, 'b) uri
  val query_lit : string -> string -> ('a, 'b) uri -> ('a, 'b) uri
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
