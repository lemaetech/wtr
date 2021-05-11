(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {2 URI} *)

(** [('a, 'b) uri] represents a URI both the path and query, e.g. /home/about/,
    /home/contact, /home/contact?name=a&no=123 etc.

    A uri is created via usage of [otr.ppx]. *)
type ('a, 'b) uri

(** [pp_uri fmt uri] pretty prints [uri] on to [fmt]. *)
val pp_uri : Format.formatter -> ('a, 'b) uri -> unit

(** {2 Route} *)

(** ['c route] is a [uri] and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route

(** [p >- route_handler] creates a route from uri [p] and [route_handler]. *)
val ( >- ) : ('a, 'b) uri -> 'a -> 'b route

(** {2 Router} *)

(** ['a t] represents a Trie based router. *)
type 'a t

(** [create routes] creates a router from a list of [route]s. *)
val create : 'a route list -> 'a t

(** [match t uri] matches a [route] to [uri], executes its handler and returns
    the computed value. [None] is returned if [uri] is not matched. *)
val match' : 'a t -> string -> 'a option

(** {2 URI Decoder} *)

(** Represents a uri component decoder, such as [:int, :float, :bool] etc.*)
type 'a decoder

(** All user defined decoders conform to the module signature decoder.

    For e.g.

    {[
      module Fruit = struct
        type t =
          | Apple
          | Orange
          | Pineapple

        let t : t Otr.decoder =
          Otr.create_decoder ~name:"fruit" ~decode:(function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None)
      end
    ]} *)
module type Decoder = sig
  type t

  val t : t decoder
end

(** [create_decoder ~name ~decode] creates a user defined decoder uri component.
    [name] is used during the pretty printing of [uri]. *)
val create_decoder : name:string -> decode:(string -> 'a option) -> 'a decoder

(**/**)

(** Only to be used by PPX *)
module Private : sig
  (** uri components *)
  val nil : ('b, 'b) uri

  val full_splat : ('b, 'b) uri

  val trailing_slash : ('b, 'b) uri

  val lit : string -> ('a, 'b) uri -> ('a, 'b) uri

  val decoder : 'a decoder -> ('b, 'c) uri -> ('a -> 'b, 'c) uri

  (** decoders *)
  val int : int decoder

  val float : float decoder

  val bool : bool decoder

  val string : string decoder
end

(**/**)
