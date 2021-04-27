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

(** ['a t] is a trie based router where ['a] is the route value. *)
type 'a t =
  | Node of
      { route : 'a option
      ; literal : 'a t String.Map.t
      ; int_param : 'a t option
      }

let empty_with route =
  Node { route; literal = String.Map.empty; int_param = None }

(** [p @-> route_handler] creates a route from path [p] and [route_handler]. *)
let ( @-> ) : ('a, 'b) Path.t -> 'a -> 'b route =
 fun path f -> Route (path, f, fun x -> x)

let empty = empty_with None

module Path_pattern = struct
  type t =
    | Literal of string
    | String
    | Int
    | Float
    | Bool

  (* [of_path path] converts [path] to [Path_pattern.t list]. This is done to
     get around some typing issue with using Path.t in the [add] function below. *)
  let rec of_path : type a b. (a, b) Path.t -> t list = function
    | Path.End -> []
    | Literal (lit, path) -> Literal lit :: of_path path
    | String path -> String :: of_path path
    | Int path -> Int :: of_path path
    | Float path -> Float :: of_path path
    | Bool path -> Bool :: of_path path
end

let add : 'b route -> 'b route t -> 'b route t =
 fun route t ->
  let (Route (path, _, _)) = route in
  let path_patterns = Path_pattern.of_path path in
  let rec loop (Node t) = function
    | [] -> Node { t with route = Some route }
    | Path_pattern.Literal lit :: path_patterns ->
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
    | Int :: path_patterns ->
      let int_param =
        let t' = Option.value t.int_param ~default:empty in
        Some (loop t' path_patterns)
      in
      Node { t with int_param }
    | _ -> assert false
  in
  loop t path_patterns

let r1 : string route =
  Path.(String (Int End)) @-> fun (s : string) (i : int) -> s ^ string_of_int i

let r2 : string route = Path.(Literal ("home", Literal ("about", End))) @-> ""

let r3 : string route =
  Path.(Literal ("home", Int End)) @-> fun (i : int) -> string_of_int i

let r4 : string route =
  Path.(Literal ("home", Float End)) @-> fun (f : float) -> string_of_float f

let router = empty |> add r1 |> add r2
