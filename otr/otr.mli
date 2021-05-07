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

val end_ : ('b, 'b) path

val string : ('a, 'b) path -> (string -> 'a, 'b) path

val int : ('a, 'b) path -> (int -> 'a, 'b) path

val float : ('a, 'b) path -> (float -> 'a, 'b) path

val bool : ('a, 'b) path -> (bool -> 'a, 'b) path

(** Variable type witness. *)
type _ ty = ..

type _ ty +=
  | Int : int ty
  | Float : float ty
  | Bool : bool ty
  | String : string ty

val arg :
     (string -> 'c option)
  -> string
  -> 'c ty
  -> ('a, 'b) path
  -> ('c -> 'a, 'b) path

(** {2 Route} *)

(** [p >- route_handler] creates a route from path [p] and [route_handler]. *)
val ( >- ) : ('a, 'b) path -> 'a -> 'b route
