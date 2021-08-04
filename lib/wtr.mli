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
type 'a t

(** ['c route] is a [uri] and its handler. ['c] represents the value returned by
    the handler. *)
and 'c route

(** [('a, 'b) uri] represents a route URI - both the path and query, e.g.
    [/home/about/,
    /home/contact, /home/contact?name=a&no=123] etc.

    A uri is created via [%wtr] ppx. *)
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

(** Represents a uri component decoder, such as [:int, :float, :bool] etc.*)
and 'a decoder

(** {1 Router} *)

val create : 'a route list list -> 'a t
(** [create routes] creates a router from a list of [route]s. Values of [routes]
    are created by [%wtr] ppx.

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

      let prod_page i = "Int page. number : " ^ string_of_int i
      let float_page f = "Float page. number : " ^ string_of_float f
      let contact_page nm num = "Contact. Hi, " ^ nm ^ ". Num " ^ string_of_int num
      let product1 name id q = Format.sprintf "Product1 %s. Id: %d. q = %b" name id q
      let product2 name id = Format.sprintf "Product2 %s. Id: %d." name id

      let fruit_page = function
        | Fruit.Apple -> "Apples are juicy!"
        | Orange -> "Orange is a citrus fruit."
        | Pineapple -> "Pineapple has scaly skin"

      let faq category_id =
        let category_name =
          match category_id with
          | 1 -> "products"
          | 2 -> "insurance"
          | 3 -> "returns"
          | _ -> "unknown"
        in
        "FAQ page for category : " ^ category_name

      let router =
        Wtr.(
          create
            [ {%wtr\| get,post,head,delete  ; /home/about/:int   \|} (fun _ ->
                  "about page" )
            ; {%wtr\| get                   ; /home/:int/        \|} prod_page
            ; {%wtr\| get,post              ; /home/:float/      \|} float_page
            ; {%wtr\| /contact/*/:int                            \|} contact_page
            ; {%wtr\| /product/:string?section=:int&q=:bool      \|} product1
            ; {%wtr\| /product/:string?section=:int&q1=yes       \|} product2
            ; {%wtr\| /fruit/:Fruit                              \|} fruit_page
            ; {%wtr\| /faq/:int/**                               \|} faq ])
    ]} *)

val match' : ?method':method' -> 'a t -> string -> 'a option
(** [match t uri] matches a [route] to [uri], executes its handler and returns
    the computed value. [None] is returned if [uri] is not matched. *)

(** {1 Decoders}

    [Wtr] provides the following built in decoders that can be used in
    [{%wtr\|\|}] ppx:

    - [:int]
    - [:int32]
    - [:int64]
    - [:float]
    - [:bool]
    - [:string]

    e.g. [{%wtr\| /home/:int \|}], [{%wtr\|/home/:bool\|}].

    Additionally creating custom, user defined decoder is also supported. The
    convention for user defined decoders is as follows:

    It should be defined in a module. The module should define a type called [t]
    and a value called [t] which returns [t Wtr.decoder].

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

val create_decoder : name:string -> decode:(string -> 'a option) -> 'a decoder
(** [create_decoder ~name ~decode] creates a user defined decoder uri component.
    [name] is used during the pretty printing of [uri]. *)

(** {1 HTTP Method} *)

val meth_equal : method' -> method' -> bool
val meth : string -> method'

(** {1 Pretty Printers} *)

val pp_method' : Format.formatter -> method' -> unit
val pp_route : Format.formatter -> 'b route -> unit
val pp : Format.formatter -> 'a t -> unit

(**/**)

(** Only to be used by PPX *)
module Private : sig
  val route : ('a, 'b) uri list -> 'a -> 'b route list
  val nil : ('b, 'b) uri

  (** uri components *)

  val full_splat : ('b, 'b) uri
  val trailing_slash : ('b, 'b) uri
  val lit : string -> ('a, 'b) uri -> ('a, 'b) uri
  val decoder : 'a decoder -> ('b, 'c) uri -> ('a -> 'b, 'c) uri
  val method' : method' -> ('a, 'b) uri -> ('a, 'b) uri

  (** decoders *)

  val int : int decoder
  val int32 : int32 decoder
  val int64 : int64 decoder
  val float : float decoder
  val bool : bool decoder
  val string : string decoder
end

(**/**)
