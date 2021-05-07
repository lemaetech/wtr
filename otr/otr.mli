(** {2 Arg} *)

(** Path argument *)
module Arg : sig
  type 'a t

  val create : name:string -> decode:(string -> 'a option) -> 'a t

  val int : int t

  val float : float t

  val bool : bool t

  val string : string t
end

(** {2 Router} *)

(** ['a t] represents a Trie based router. *)
type 'a t

(** ['c route] is a [path] and its handler. ['c] represents the value returned
    by the handler. *)
type 'c route

(** [create routes] creates a router from a list of [route]s. *)
val create : 'a route list -> 'a t

(** [match t path] matches a [route] to [path], executes its handler and returns
    the computed value. [None] is returned is [path] is not matched. *)
val match' : 'a t -> string -> 'a option

(** {2 URI} *)

(** [('a, 'b) path] represents a HTTP URI path, eg. /home/about/, /home/contact,
    etc. *)
type ('a, 'b) path

(** {2 Route} *)

(** [p >- route_handler] creates a route from path [p] and [route_handler]. *)
val ( >- ) : ('a, 'b) path -> 'a -> 'b route
