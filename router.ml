module Path = struct
  type ('ty, 'v) t =
    | End : ('v, 'v) t
    | Literal : string * ('ty, 'v) t -> ('ty, 'v) t
    | String : ('ty, 'v) t -> (string -> 'ty, 'v) t
    | Int : ('ty, 'v) t -> (int -> 'ty, 'v) t
    | Float : ('ty, 'v) t -> (int -> 'ty, 'v) t
    | Bool : ('ty, 'v) t -> (bool -> 'ty, 'v) t
end

module Route = struct
  type (_, _) t =
    | End : ('a, 'a) t
    | Compose : ('a, 'b) Path.t * ('b, 'c) t -> ('a, 'c) t
end

module R = Route
module P = Path
module Smap = Map.Make (String)

type route_handler = Handler : ('ty, 'v) Route.t * 'ty -> route_handler

type t =
  | Node of
      { data : route_handler option
      ; literal : t Smap.t
      ; int_param : t option
      }

let empty_with data = Node { data; literal = Smap.empty; int_param = None }

let empty = empty_with None

let add : type ty v. t -> (ty, v) Route.t -> ty -> t =
 fun (Node node) route f -> Node { node with data = Some (Handler (route, f)) }

(* /home/:string/:int*)
let r1 = R.Compose (P.Literal ("home", P.String (P.Int P.End)), R.End)

(* /home/:string/:string *)
let r2 = R.Compose (P.Literal ("home", P.String (P.String P.End)), R.End)

let router1 = add empty r1 (fun s1 i -> s1 ^ " || " ^ string_of_int i)

let route1 = add router1 r2 (fun s1 s2 -> s1 ^ " || " ^ s2)
