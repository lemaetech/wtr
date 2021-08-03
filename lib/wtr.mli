(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 URI} *)

(** [('a, 'b) uri] represents a URI - both the path and query, e.g.
    [/home/about/,
    /home/contact, /home/contact?name=a&no=123] etc.

    A uri is created via usage of [wtr.ppx]. *)
type ('a, 'b) uri

val pp_uri : Format.formatter -> ('a, 'b) uri -> unit
(** [pp_uri fmt uri] pretty prints [uri] on to [fmt]. *)

(** {2 HTTP method *)

(** [meth] represents HTTP request methods. It can be used as part of a
    {!type:route}. *)
type meth =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]

val meth_equal : meth -> meth -> bool
val meth : string -> meth
val pp_meth : Format.formatter -> meth -> unit

(** {2 Route} *)

(** ['c route] is a [uri] and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route

val route : meth list -> ('a, 'b) uri -> 'a -> 'b route
(** [route methods uri handler] creates a route which matches request method
    [meth], [uri] and a route handler [handler]. *)

val pp_route : Format.formatter -> 'b route -> unit

val ( >- ) : ('a, 'b) uri -> 'a -> 'b route
(** [p >- handler] creates a route from uri [p] and a route handler [handler].
    The resulting route is not constrained by HTTP methods,i.e. it accepts all
    HTTP methods. *)

(** {2 Router} *)

(** ['a t] represents a Trie based router. *)
type 'a t

val create : 'a route list -> 'a t
(** [create routes] creates a router from a list of [route]s. *)

val match' : ?meth:meth -> 'a t -> string -> 'a option
(** [match t uri] matches a [route] to [uri], executes its handler and returns
    the computed value. [None] is returned if [uri] is not matched. *)

val pp : Format.formatter -> 'a t -> unit

(** {2 Decoder} *)

(** Represents a uri component decoder, such as [:int, :float, :bool] etc.*)
type 'a decoder

(** All user defined decoders conform to the module signature decoder.

    For e.g.

    {[
      module Fruit = struct
        type t = Apple | Orange | Pineapple

        let t : t Wtr.decoder =
          Wtr.create_decoder ~name:"fruit" ~decode:(function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None )
      end
    ]} *)
module type Decoder = sig
  type t

  val t : t decoder
end

val create_decoder : name:string -> decode:(string -> 'a option) -> 'a decoder
(** [create_decoder ~name ~decode] creates a user defined decoder uri component.
    [name] is used during the pretty printing of [uri]. *)

(**/**)

(** Only to be used by PPX *)
module Private : sig
  val nil : ('b, 'b) uri
  (** uri components *)

  val full_splat : ('b, 'b) uri
  val trailing_slash : ('b, 'b) uri
  val lit : string -> ('a, 'b) uri -> ('a, 'b) uri
  val decoder : 'a decoder -> ('b, 'c) uri -> ('a -> 'b, 'c) uri

  val int : int decoder
  (** decoders *)

  val int32 : int32 decoder
  val int64 : int64 decoder
  val float : float decoder
  val bool : bool decoder
  val string : string decoder
end

(**/**)
