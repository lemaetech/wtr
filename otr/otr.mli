type _ ty = ..

type _ ty +=
  | Int : int ty
  | Float : float ty
  | Bool : bool ty
  | String : string ty

type ('a, 'b) uri

val end_ : ('b, 'b) uri

val string : ('a, 'b) uri -> (string -> 'a, 'b) uri

val int : ('a, 'b) uri -> (int -> 'a, 'b) uri

val float : ('a, 'b) uri -> (float -> 'a, 'b) uri

val bool : ('a, 'b) uri -> (bool -> 'a, 'b) uri

val var :
  (string -> 'c option) -> string -> 'c ty -> ('a, 'b) uri -> ('c -> 'a, 'b) uri

(** {2 Route} *)

(** ['c route] is a uri and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route

(** [p >- route_handler] creates a route from uri [p] and [route_handler]. *)
val ( >- ) : ('a, 'b) uri -> 'a -> 'b route

(** {2 Router} *)

(** ['a t] represents a a Trie based router. *)
type 'a t

(** [add route t] adds a [route] to a router [t]. *)
val add : 'a route -> 'a t -> 'a t

(** Represents a router that is ready to be matched. *)
type 'a t_compiled

(** [compile t] returns a compiled router ready for matching. *)
val compile : 'a t -> 'a t_compiled

(** [match t uri] matches a [route] to [uri], executes its handler and returns
    the computed value. [None] is returned if [uri] is not matched. *)
val match' : 'a t_compiled -> string -> 'a option
