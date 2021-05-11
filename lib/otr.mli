(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
*-------------------------------------------------------------------------*)

(** {2 URI uri} *)

(** [('a, 'b) uri] represents a HTTP URI uri, eg. /home/about/, /home/contact,
    etc. *)
type ('a, 'b) uri

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

(** {2 URI argument} *)

(** Represents a uri argument, such as [:int, :float, :bool] etc.*)
type 'a arg

(** [create_arg ~name ~decode] creates a user specified argument uri component. *)
val create_arg : name:string -> decode:(string -> 'a option) -> 'a arg

(** All user defined args conform to the module signature Arg.

    For e.g.

    {[
      module Fruit = struct
        type t =
          | Apple
          | Orange
          | Pineapple

        let t : t Otr.arg =
          Otr.create_arg ~name:"fruit" ~decode:(function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None)
      end
    ]} *)
module type Arg = sig
  type t

  val t : t arg
end

(**/**)

(** Only to be used by PPX *)
module Private : sig
  (** uri components *)
  val nil : ('b, 'b) uri

  val full_splat : ('b, 'b) uri

  val trailing_slash : ('b, 'b) uri

  val lit : string -> ('a, 'b) uri -> ('a, 'b) uri

  val arg : 'a arg -> ('b, 'c) uri -> ('a -> 'b, 'c) uri

  (** Args *)
  val int : int arg

  val float : float arg

  val bool : bool arg

  val string : string arg
end

(**/**)
