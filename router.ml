open! Core

module Path = struct
  type ('a, 'b) t =
    | End : ('b, 'b) t
    | Literal : string * ('a, 'b) t -> ('a, 'b) t
    | String : ('a, 'b) t -> (string -> 'a, 'b) t
    | Int : ('a, 'b) t -> (int -> 'a, 'b) t
    | Float : ('a, 'b) t -> (float -> 'a, 'b) t
    | Bool : ('a, 'b) t -> (bool -> 'a, 'b) t
end

type 'b route = Route : ('a, 'c) Path.t * 'a * ('c -> 'b) -> 'b route

(** Router *)
type 'b t =
  | Node of
      { route : 'b route option
      ; literal : 'b t String.Map.t
      ; int_param : 'b t option
      }

let empty_with route =
  Node { route; literal = String.Map.empty; int_param = None }

(** [p @-> route_handler] creates a route from path [p] and [route_handler]. *)
let ( @-> ) : ('a, 'b) Path.t -> 'a -> 'b route =
 fun path f -> Route (path, f, fun x -> x)

let empty () = empty_with None

let r1 : string route =
  Path.(String (Int End)) @-> fun (s : string) (i : int) -> s ^ string_of_int i

let r2 : string route = Path.(Literal ("home", Literal ("about", End))) @-> ""

let r3 : string route =
  Path.(Literal ("home", Int End)) @-> fun (i : int) -> string_of_int i

let r4 : string route =
  Path.(Literal ("home", Float End)) @-> fun (f : float) -> string_of_float f

(* let add : 'b route -> 'b t -> 'b t = *)
(*  fun (Route (path, _, _) as r) t -> *)
(*   let loop (Node _node) = function *)
(*     | Path.End -> empty_with (Some r) *)
(*     | _ -> assert false *)
(*   in *)
(*   loop t path *)

(* let match_uri : t -> string -> 'a option = fun _router uri -> Some uri*)

(* (* /home/:string/:int*)*)
(* let r1 : (string -> int -> 'a, 'a) Route.t =*)
(*   R.[ P.Literal "home"; P.String; P.Int ]*)

(* (* /home/:string/:string *)*)
(* let r2 : (string -> string -> 'a, 'a) Route.t =*)
(*   R.[ P.Literal "home"; P.String; P.String ]*)

(* let router =*)
(*   empty*)
(*   |> add r1 (fun (s1 : string) (i : int) -> s1 ^ " || " ^ string_of_int i)*)
(*   |> add r2 (fun (s1 : string) (s2 : string) -> s1 ^ " || " ^ s2)*)
