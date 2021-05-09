(** [('a, 'b) path] represents a HTTP URI path, eg. /home/about/, /home/contact,
    etc. *)
type ('a, 'b) path

(** Path argument *)
type 'a arg

(** [create_arg ~name ~decode] creates a user specified argument path component. *)
val create_arg : name:string -> decode:(string -> 'a option) -> 'a arg

(** ['c route] is a [path] and its handler. ['c] represents the value returned
    by the handler. *)
type 'c route

(** ['a t] represents a Trie based router. *)
type 'a t

(** [create routes] creates a router from a list of [route]s. *)
val create : 'a route list -> 'a t

(** [match t path] matches a [route] to [path], executes its handler and returns
    the computed value. [None] is returned is [path] is not matched. *)
val match' : 'a t -> string -> 'a option

(** [p >- route_handler] creates a route from path [p] and [route_handler]. *)
val ( >- ) : ('a, 'b) path -> 'a -> 'b route

(**/**)

(** Only to be used by PPX *)
module Private : sig
  (** Path components *)
  val nil : ('b, 'b) path

  val full_splat : ('b, 'b) path

  val trailing_slash : ('b, 'b) path

  val lit : string -> ('a, 'b) path -> ('a, 'b) path

  val arg : 'a arg -> ('b, 'c) path -> ('a -> 'b, 'c) path

  (** Args *)
  val int : int arg

  val float : float arg

  val bool : bool arg

  val string : string arg
end

(**/**)
