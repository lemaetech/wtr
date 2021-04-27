open! Core

module Path = struct
  type ('a, 'b) t = ..

  type ('a, 'b) t +=
    | End : ('b, 'b) t
    | Literal : string * ('a, 'b) t -> ('a, 'b) t
    | String : ('a, 'b) t -> (string -> 'a, 'b) t
    | Int : ('a, 'b) t -> (int -> 'a, 'b) t
    | Float : ('a, 'b) t -> (float -> 'a, 'b) t
    | Bool : ('a, 'b) t -> (bool -> 'a, 'b) t

  (** Path pattern - Monomorphic version of [t]. *)
  type pattern =
    | PLiteral of string
    | PString
    | PInt
    | PFloat
    | PBool

  (* [of_path path] converts [path] to [Path_pattern.t list]. This is done to
     get around some typing issue with using Path.t in the [add] function below. *)
  let rec pattern : type a b. (a, b) t -> pattern list = function
    | End -> []
    | Literal (lit, path) -> PLiteral lit :: pattern path
    | String path -> PString :: pattern path
    | Int path -> PInt :: pattern path
    | Float path -> PFloat :: pattern path
    | Bool path -> PBool :: pattern path
    | _ -> assert false
end

type 'c route = Route : ('a, 'c) Path.t * 'a -> 'c route

(** ['a t] is a trie based router where ['a] is the route value. *)
type 'a t =
  | Node of
      { route : 'a route option
      ; literal : 'a t String.Map.t
      ; int_param : 'a t option
      }

let empty_with route =
  Node { route; literal = String.Map.empty; int_param = None }

(** [p @-> route_handler] creates a route from path [p] and [route_handler]. *)
let ( @-> ) : ('a, 'b) Path.t -> 'a -> 'b route = fun path f -> Route (path, f)

let empty = empty_with None

module Path_pattern = struct end

let add : 'b route -> 'b t -> 'b t =
 fun route t ->
  let (Route (path, _)) = route in
  let rec loop : 'b t -> Path.pattern list -> 'b t =
   fun (Node t) -> function
    | [] -> Node { t with route = Some route }
    | PLiteral lit :: path_patterns ->
      let literal =
        match String.Map.find t.literal lit with
        | Some t' ->
          String.Map.change t.literal lit ~f:(function
              | Some _
              | None
              -> Some (loop t' path_patterns))
        | None ->
          String.Map.add_exn t.literal ~key:lit ~data:(loop empty path_patterns)
      in
      Node { t with literal }
    | PInt :: path_patterns ->
      let int_param =
        let t' = Option.value t.int_param ~default:empty in
        Some (loop t' path_patterns)
      in
      Node { t with int_param }
    | _ -> assert false
  in
  loop t (Path.pattern path)

(* let match': 'b route t -> string -> 'b option = *)

let r1 =
  Path.(String (Int End)) @-> fun (s : string) (i : int) -> s ^ string_of_int i

let r2 : string route = Path.(Literal ("home", Literal ("about", End))) @-> ""

let r3 : string route =
  Path.(Literal ("home", Int End)) @-> fun (i : int) -> string_of_int i

let r4 : string route =
  Path.(Literal ("home", Float End)) @-> fun (f : float) -> string_of_float f

(** This should give error (we added an extra () param in handler) but it
    doesn't. It only errors when adding to the router.*)
let r5 =
  Path.(String (Int End))
  @-> fun (s : string) (i : int) () -> s ^ string_of_int i

let router = empty |> add r1 |> add r2 |> add r3 |> add r4

(* |> add r5  *)
(* This errors *)
