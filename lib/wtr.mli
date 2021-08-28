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
    /home/contact, /home/contact?name=a&no=123] etc. *)
and ('a, 'b) uri =
  | End : ('b, 'b) uri
  | Splat : (string -> 'b, 'b) uri
  | Trailing_slash : ('b, 'b) uri
  | Literal : string * ('a, 'b) uri -> ('a, 'b) uri
  | Decode : 'c decoder * ('a, 'b) uri -> ('c -> 'a, 'b) uri

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

(** {1 URI} *)

val end' : ('b, 'b) uri
val splat : (string -> 'b, 'b) uri
val trailing_slash : ('b, 'b) uri
val lit : string -> ('a, 'b) uri -> ('a, 'b) uri
val decode : 'a decoder -> ('b, 'c) uri -> ('a -> 'b, 'c) uri
(* val int : (int -> 'b, 'c) uri *)

(** {1 Route} *)

val route : method' -> ('a, 'b) uri -> 'a -> 'b route
val routes : method' list -> ('a, 'b) uri -> 'a -> 'b route list

(** {1 Router} *)

val wtr : 'a route list list -> 'a t
(** [create routes] creates a router from a list of [route]s. Values of [routes]
    are created by [%wtr] ppx.

    A full example demonstrating creating a router, route and route handlers:

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

      (* Route handlers. *)
      let about_page = "about page"
      let prod_page i = "Int page. number : " ^ string_of_int i
      let float_page f = "Float page. number : " ^ string_of_float f

      let contact_page nm num =
        "Contact. Hi, " ^ nm ^ ". Num " ^ string_of_int num

      let product1 name id q =
        Format.sprintf "Product1 %s. Id: %d. q = %b" name id q

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
        Wtr.create
          [ {%wtr| get,post,head,delete  ; /home/about/       |} about_page
          ; {%wtr| head,delete           ; /home/:int/        |} prod_page
          ; {%wtr| get,post              ; /home/:float/      |} float_page
          ; {%wtr| get; /contact/*/:int                       |} contact_page
          ; {%wtr| get; /product/:string?section=:int&q=:bool |} product1
          ; {%wtr| get; /product/:string?section=:int&q1=yes  |} product2
          ; {%wtr| get; /fruit/:Fruit                         |} fruit_page
          ; {%wtr| GET; /faq/:int/**                          |} faq ]
    ]} *)

val match' : method' -> string -> 'a t -> 'a option
(** [match method' uri t] matches a route to a given [uri] and [method'],
    executes its handler and returns the computed value. [None] is returned if
    both [uri] {b and} [method'] are not matched.

    Examples of calling [match'] and its results:

    {[
      let () =
        Format.(fprintf std_formatter "@.@.====Router Match Results====@.") ;
        [ Wtr.match' `GET "/home/100001.1/" router
        ; Wtr.match' `DELETE "/home/100001/" router
        ; Wtr.match' `GET "/home/about/" router
        ; Wtr.match' `GET "/product/dyson350?section=233&q=true" router
        ; Wtr.match' `GET "/product/dyson350?section=2&q=false" router
        ; Wtr.match' `GET "/product/dyson350?section=2&q1=yes" router
        ; Wtr.match' `GET "/product/dyson350?section=2&q1=no" router
        ; Wtr.match' `GET "/fruit/apple" router
        ; Wtr.match' `GET "/fruit/orange" router
        ; Wtr.match' `GET "/fruit/pineapple" router
        ; Wtr.match' `GET "/fruit/guava" router
        ; Wtr.match' `GET "/faq/1/" router
        ; Wtr.match' `GET "/faq/1/whatever" router
        ; Wtr.match' `GET "/faq/2/whateasdfasdfasdf" router ]
        |> List.iteri (fun i -> function
             | Some s -> Printf.printf "%3d: %s\n" (i + 1) s
             | None -> Printf.printf "%3d: None\n" (i + 1) )
    ]}

    The match call results in the following results:

    {v
====Router Match Results====
 1: Float page. number : 100001.1
 2: Int page. number : 100001
 3: about page
 4: Product1 dyson350. Id: 233. q = true
 5: Product1 dyson350. Id: 2. q = false
 6: Product2 dyson350. Id: 2.
 7: None
 8: Apples are juicy!
 9: Orange is a citrus fruit.
10: Pineapple has scaly skin
11: None
12: FAQ page for category : products
13: FAQ page for category : products
14: FAQ page for category : insurance
    v} *)

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
        Wtr.create
          [{%wtr|get; /public/** |} (fun url -> Format.sprintf "%s" url)]
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

    [{%wtr|get; /home/:int |}], [{%wtr| /home/:bool |}] *)

(** {2 Custom Decoders}

    Wtr also supports creating custom, user defined decoders. The convention for
    user defined decoders is as follows:

    It should be defined in a module. The module should define a type called [t]
    and a value called [t] which returns [t Wtr.decoder].

    Example of defining custom decoder:

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
    ]}

    The custom decoder then can be used in [%wtr] ppx as follows,

    [{%wtr| get ; /fruit/:Fruit  |} fruit_page] *)

(** {2 Decoders and Route Handlers}

    Usage of decoders in a URI directly affect the function signature of a route
    handler. For e.g.

    - A uri spec [/home/:int/:bool] expects a route handler as
      [fun (i:int) (b:bool) -> ....]

    - A uri spec [/home/:string] expects a route handler as
      [(fun (s:string) -> ...)] *)

val decoder : name:string -> decode:(string -> 'a option) -> 'a decoder
(** [create_decoder ~name ~decode] creates a user defined decoder uri component.
    [name] is used during the pretty printing of [uri]. *)

val int_d : int decoder
val int32_d : int32 decoder
val int64_d : int64 decoder
val float_d : float decoder
val bool_d : bool decoder
val string_d : string decoder

(** {1 HTTP Method} *)

val method_equal : method' -> method' -> bool

val method' : string -> method'
(** [method' m] creates a {!type:method'} from value [m]. *)

(** {1:pp Pretty Printers} *)

val pp : Format.formatter -> 'a t -> unit
(** [pp fmt t] pretty prints router routes. This can be useful for debugging
    router/routing issues as it displays hierarchially possible routes a
    matching engine may take in matching a given uri and method.

    HTTP method names are capitalized.

    Printing the [router] from the example given in {!val:create} method pretty
    prints the following to the terminal console:

    [Wtr.pp Format.std_formatter router;;]

    {[
      GET
        /home
          /about
            /
          /:float
            /
        /contact
          /:string
            /:int
        /product
          /:string
            /section
              /:int
                /q
                  /:bool
                /q1
                  /yes
        /fruit
          /:fruit
        /faq
          /:int
            /**
      POST
        /home
          /about
            /
          /:float
            /
      HEAD
        /home
          /about
            /
          /:int
            /
      DELETE
        /home
          /about
            /
          /:int
            /
    ]} *)

val pp_method : Format.formatter -> method' -> unit
val pp_route : Format.formatter -> 'b route -> unit
