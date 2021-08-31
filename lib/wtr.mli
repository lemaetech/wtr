(*-------------------------------------------------------------------------
 * Copyright (c) 2021 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 *-------------------------------------------------------------------------*)

(** {1 Types} *)

(** ['a t] represents a Trie based router. *)
type 'a router

(** ['c route] is a [uri] and its handler. ['c] represents the value returned by
    the handler. *)
and 'c route

(** [('a, 'b) uri] represents a route URI - both the path and query, e.g.
    [/home/about/,
    /home/contact, /home/contact?name=a&no=123] etc. *)
and ('a, 'b) uri

(** [method'] represents HTTP request methods. It can be used as part of a
    {!type:uri} in [%wtr] ppx. *)
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

(** Represents a uri component decoder, such as [:int, :float, :bool] etc. *)
and 'a decoder

(** {1 Route} *)

val route : ?method':method' -> ('a, 'b) uri -> 'a -> 'b route
val routes : method' list -> ('a, 'b) uri -> 'a -> 'b route list

(** {1 Router} *)

val router : 'a route list list -> 'a router
val match' : method' -> string -> 'a router -> 'a option

(** {1:uri Specifying a URI}

    Specifying a URI in a [%wtr] ppx follows the following syntax:

    [wtr uri spec = http methods separated by comma ';' http uri]

    A URI in a [%wtr] ppx is syntactically and sematically a HTTP URI with the
    addition of decoders and some some useful additions listed below.

    + {b Full splat [**]} - Full spat operator matches any/all path following a
      full splat. For example in [/home/**] matches the following uri paths,
      [/home/about/, home/contact, /home/product] etc. Full splat must be the
      last component of an uri. It is an error to specify other uri path
      component after full splat operator. Additionally [wtr] decodes the
      remaining matched url in the route handler. For example,

    {[
      let r =
        Wtr.t [{%wtr|get; /public/** |} (fun url -> Format.sprintf "%s" url)]
      in
      let s = Wtr.match' `GET "/public/css/style.css" in
      s = Some "css/style.css"
    ]}
    + {b Wildward [*]} - A wildcard operator matches any text appearing on the
      path component position. For example, uri [/home/*/page1] matches the
      following [/home/23/page1, /home/true/page1, /home/234.4/page1] etc. The
      semantics of wildcard operator is the same as using [:string] decoder in a
      uri, i.e. it affects the route handler function signature.
    + {b Trailing slash [/]} - A trailing slash ensures that Wtr will match a
      trailing [/] in a uri. For example, uri [/home/about/] matches
      [/home/about/] but not [/home/about]. *)

(** {1:decoders Decoders}

    {2 Built-in Decoders}

    [Wtr] provides the following built in decoders that can be used as when
    specifying wtr URI in [{%wtr| |}] ppx:

    - [:int] - decodes a [int]
    - [:int32] - decodes a [int32]
    - [:int64] - decodes a [int64]
    - [:float] - decodes a [float] or [int]
    - [:bool] - decodes a [bool]
    - [:string] - decodes a [string]

    The built-in decoders can be used as follows:

    [{%wtr|get; /home/:int |}], [{%wtr| /home/:bool |}]

    {2 Custom Decoders}

    Wtr also supports creating custom, user defined decoders. The convention for
    user defined decoders is as follows:

    It should be defined in a module. The module should define a type called [t]
    and a value called [t] which returns [t Wtr.decoder].

    Example of defining custom decoder:

    {[
      module Fruit = struct
        type t = Apple | Orange | Pineapple

        let t : t Wtr.decoder =
          Wtr.decoder ~name:"fruit" ~decode:(function
            | "apple" -> Some Apple
            | "orange" -> Some Orange
            | "pineapple" -> Some Pineapple
            | _ -> None )
      end
    ]}

    The custom decoder then can be used in [%wtr] ppx as follows,

    [{%wtr| get ; /fruit/:Fruit  |} fruit_page] *)

(** {2 Route Handlers}

    Usage of decoders in a URI directly affect the function signature of a route
    handler. For e.g.

    - A uri spec [/home/:int/:bool] expects a route handler as
      [fun (i:int) (b:bool) -> ....]

    - A uri spec [/home/:string] expects a route handler as
      [(fun (s:string) -> ...)] *)

val decoder : name:string -> decode:(string -> 'a option) -> 'a decoder
(** [create_decoder ~name ~decode] creates a user defined decoder uri component.
    [name] is used during the pretty printing of [uri]. *)

(** {1 HTTP Method} *)

val method_equal : method' -> method' -> bool
val method' : string -> method'

(** {1:pp Pretty Printers} *)

val pp : Format.formatter -> 'a router -> unit
val pp_method : Format.formatter -> method' -> unit
val pp_route : Format.formatter -> 'b route -> unit
val pp_uri : Format.formatter -> ('a, 'b) uri -> unit

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
