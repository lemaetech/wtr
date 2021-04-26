open! Core

module Path = struct
  type ('ty, 'v) t =
    | Literal : string -> ('ty, 'v) t
    | String : (string -> 'ty, 'v) t
    | Int : (int -> 'ty, 'v) t
    | Float : (float -> 'ty, 'v) t
    | Bool : (bool -> 'ty, 'v) t
end

module Route = struct
  type (_, _) t =
    | [] : ('a, 'b) t
    | ( :: ) : ('a, 'b) Path.t * ('b, 'c) t -> ('a, 'c) t
end

type route_handler = Handler : ('ty, 'v) Route.t * 'ty -> route_handler

(** Router *)
type t =
  | Node of
      { route_handler : route_handler option
      ; literal : t String.Map.t
      ; int_param : t option
      }

let empty_with route_handler =
  Node { route_handler; literal = String.Map.empty; int_param = None }

let empty = empty_with None

module R = Route
module P = Path

let add : type ty v. (ty, v) Route.t -> ty -> t -> t =
 fun route f t ->
  let loop (Node _node) = function
    | R.[] -> empty_with (Some (Handler (route, f)))
    | R.(_ :: _) -> empty_with (Some (Handler (route, f)))
  in
  loop t route

let match_uri : t -> string -> 'a option = fun _router uri -> Some uri

(* /home/:string/:int*)
let r1 : (string -> int -> 'a, 'a) Route.t =
  R.[ P.Literal "home"; P.String; P.Int ]

(* /home/:string/:string *)
let r2 : (string -> string -> 'a, 'a) Route.t =
  R.[ P.Literal "home"; P.String; P.String ]

let router =
  empty
  |> add r1 (fun (s1 : string) (i : int) -> s1 ^ " || " ^ string_of_int i)
  |> add r2 (fun (s1 : string) (s2 : string) -> s1 ^ " || " ^ s2)
