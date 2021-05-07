(** {2 Router} *)

(** ['a t] represents a Trie based router. *)
type 'a t

(** [('a, 'b) uri] represents a uniform resource identifier, eg. /home/about/,
    /home/contact, etc. *)
type ('a, 'b) uri

(** ['c route] is a uri and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route

(** [create routes] creates a router from a list of [route]s. *)
val create : 'a route list -> 'a t

(** [match t uri] matches a [route] to [uri], executes its handler and returns
    the computed value. [None] is returned is [uri] is not matched. *)
val match' : 'a t -> string -> 'a option

(** [p >- route_handler] creates a route from uri [p] and [route_handler]. *)
val ( >- ) : ('a, 'b) uri -> 'a -> 'b route

(** {2 URI} *)

val end_ : ('b, 'b) uri

val string : ('a, 'b) uri -> (string -> 'a, 'b) uri

val int : ('a, 'b) uri -> (int -> 'a, 'b) uri

val float : ('a, 'b) uri -> (float -> 'a, 'b) uri

val bool : ('a, 'b) uri -> (bool -> 'a, 'b) uri

(** Variable type witness. *)
type _ ty = ..

type _ ty +=
  | Int : int ty
  | Float : float ty
  | Bool : bool ty
  | String : string ty

val var :
  (string -> 'c option) -> string -> 'c ty -> ('a, 'b) uri -> ('c -> 'a, 'b) uri
