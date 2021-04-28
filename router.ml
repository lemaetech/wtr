open! Core

module Path = struct
  type ('a, 'b) t =
    | End : ('b, 'b) t
    | Literal : string * ('a, 'b) t -> ('a, 'b) t
        (** Literal path component eg. 'home' in '/home' *)
    | Param : 'c param * ('a, 'b) t -> ('c -> 'a, 'b) t
        (** Parameter path component eg. ':int' in '/home/:int' *)

  (* Parameter detail. *)
  and 'c param =
    { decode : string -> 'c option
    ; encode : 'c -> string
    ; name : string (* name e.g. :int, :float, :bool, :string etc *)
    }

  let end_ : ('b, 'b) t = End

  let lit : string -> ('a, 'b) t -> ('a, 'b) t = fun s path -> Literal (s, path)

  let param decode encode name path = Param ({ encode; decode; name }, path)

  let string : ('a, 'b) t -> (string -> 'a, 'b) t =
   fun path -> param (fun s -> Some s) Fun.id ":string" path

  let int : ('a, 'b) t -> (int -> 'a, 'b) t =
   fun path -> param int_of_string_opt string_of_int ":int" path

  let float : ('a, 'b) t -> (float -> 'a, 'b) t =
   fun path -> param float_of_string_opt string_of_float ":float" path

  let bool : ('a, 'b) t -> (bool -> 'a, 'b) t =
   fun path -> param bool_of_string_opt string_of_bool ":bool" path

  (** [kind] encodes path kind/type. *)
  type kind =
    | KLiteral : string -> kind
    | KParam : 'c param -> kind

  (* [kind path] converts [path] to [kind list]. This is done to get around
     OCaml type inference issue when using [Path.t] type in the [add] function
     below. *)
  let rec kind : type a b. (a, b) t -> kind list = function
    | End -> []
    | Literal (lit, path) -> KLiteral lit :: kind path
    | Param (conv, path) -> KParam conv :: kind path
end

(** ['c route] is a path and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route = Route : ('a, 'c) Path.t * 'a -> 'c route

(** [p @-> route_handler] creates a route from path [p] and [route_handler]. *)
let ( @-> ) : ('a, 'b) Path.t -> 'a -> 'b route = fun path f -> Route (path, f)

(** ['a t] is a trie based router where ['a] is the route value. *)
type 'a t =
  | Node of
      { route : 'a route option
      ; literals : 'a t String.Map.t
      ; params : 'a t String.Map.t
      }

let empty_with route =
  Node { route; literals = String.Map.empty; params = String.Map.empty }

let empty = empty_with None

let add : 'b route -> 'b t -> 'b t =
 fun route t ->
  let (Route (path, _)) = route in
  let rec loop : 'b t -> Path.kind list -> 'b t =
   fun (Node t) kinds ->
    let add_update m key kinds =
      match String.Map.find m key with
      | Some t' ->
        String.Map.change m key ~f:(function
            | Some _
            | None
            -> Some (loop t' kinds))
      | None -> String.Map.add_exn m ~key ~data:(loop empty kinds)
    in
    match kinds with
    | [] -> Node { t with route = Some route }
    | KLiteral lit :: kinds ->
      let literals = add_update t.literals lit kinds in
      Node { t with literals }
    | KParam p :: kinds ->
      let params = add_update t.params p.name kinds in
      Node { t with params }
  in
  loop t (Path.kind path)

let rec apply : type a b. (a, b) Path.t -> a -> string list -> b =
 fun path f params ->
  match (path, params) with
  | End, [] -> f
  | Literal (_, path), params -> apply path f params
  | Param (conv, path), p :: params -> (
    match conv.decode p with
    | Some p' -> apply path (f p') params
    | None -> failwith "Route not matched")
  | _, _ -> failwith "Route not matched"

let match' : 'b t -> string -> 'b option =
 fun t uri ->
  let tokens =
    String.rstrip uri ~drop:(function
      | '/' -> true
      | _ -> false)
    |> String.split ~on:'/'
  in
  let loop (Node t) params tokens =
    match tokens with
    | [] -> Option.map t.route ~f:(fun (Route (path, f)) -> apply path f params)
    | _ -> assert false
  in
  loop t [] tokens

(* None *)

let r1 =
  Path.(string (int end_)) @-> fun (s : string) (i : int) -> s ^ string_of_int i

let r2 : string route = Path.(lit "home" (lit "about" end_)) @-> ""

let r3 : string route =
  Path.(lit "home" (int end_)) @-> fun (i : int) -> string_of_int i

let r4 : string route =
  Path.(lit "home" (float end_)) @-> fun (f : float) -> string_of_float f

(** This should give error (we added an extra () param in handler) but it
    doesn't. It only errors when adding to the router.*)
let r5 =
  Path.(string (int end_))
  @-> fun (s : string) (i : int) () -> s ^ string_of_int i

let router = empty |> add r1 |> add r2 |> add r3 |> add r4

(* |> add r5  *)
(* This errors *)
