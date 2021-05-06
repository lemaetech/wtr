open! Core

(** Variable type. *)
module Var_ty = struct
  type _ t = ..

  type _ t += Int : int t | Float : float t | Bool : bool t | String : string t

  type (_, _) eq = Eq : ('a, 'a) eq

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun atid btid ->
    match (atid, btid) with
    | Int, Int -> Some Eq
    | Float, Float -> Some Eq
    | Bool, Bool -> Some Eq
    | String, String -> Some Eq
    | _ -> None
end

(** [('a, 'b) uri] represents a uniform resource identifier. The variant members
    describe the uri path component types. *)
type ('a, 'b) uri =
  | End : ('b, 'b) uri
  | Literal : string * ('a, 'b) uri -> ('a, 'b) uri
      (** Uri literal string component eg. 'home' in '/home' *)
  | Var : 'c var * ('a, 'b) uri -> ('c -> 'a, 'b) uri
      (** Uri variable component, i.e. the value is determined during runtime,
          eg. ':int' in '/home/:int' *)

and 'c var =
  { decode : string -> 'c option
  ; name : string (* name e.g. int, float, bool, string etc *)
  ; tid : 'c Var_ty.t
  }

let end_ : ('b, 'b) uri = End

let lit : string -> ('a, 'b) uri -> ('a, 'b) uri = fun s uri -> Literal (s, uri)

let var decode name tid uri = Var ({ decode; name; tid }, uri)

let string : ('a, 'b) uri -> (string -> 'a, 'b) uri =
 fun uri -> var (fun s -> Some s) "string" Var_ty.String uri

let int : ('a, 'b) uri -> (int -> 'a, 'b) uri =
 fun uri -> var int_of_string_opt "int" Var_ty.Int uri

let float : ('a, 'b) uri -> (float -> 'a, 'b) uri =
 fun uri -> var float_of_string_opt "float" Var_ty.Float uri

let bool : ('a, 'b) uri -> (bool -> 'a, 'b) uri =
 fun uri -> var bool_of_string_opt "bool" Var_ty.Bool uri

(** [uri_kind] Existential which encodes uri kind/type. *)
type uri_kind =
  | KLiteral : string -> uri_kind
  | KVar : 'c var -> uri_kind

(* [kind uri] converts [uri] to [kind list]. This is done to get around OCaml
   type inference issue when using [uri] type in the [add] function below. *)
let rec uri_kind : type a b. (a, b) uri -> uri_kind list = function
  | End -> []
  | Literal (lit, uri) -> KLiteral lit :: uri_kind uri
  | Var (var, uri) -> KVar var :: uri_kind uri

(** ['c route] is a uri and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route = Route : ('a, 'c) uri * 'a -> 'c route

(** [p >- route_handler] creates a route from uri [p] and [route_handler]. *)
let ( >- ) : ('a, 'b) uri -> 'a -> 'b route = fun uri f -> Route (uri, f)

(** ['a t] is a node in a trie based router. *)
type 'a t =
  { route : 'a route option (* ; literals : 'a t String.Map.t *)
  ; path : (uri_kind * 'a t) Queue.t
  }

let empty_with route = { route; path = Queue.create () }

let empty = empty_with None

let add (Route (uri, _) as route) t =
  let rec loop : 'b t -> uri_kind list -> 'b t =
   fun t uri_kinds ->
    match uri_kinds with
    | [] -> { t with route = Some route }
    | (KLiteral lit as kvar) :: uri_kinds ->
      update_path
        ~f:(function
          | KLiteral lit', _ -> String.equal lit lit'
          | _ -> false)
        t kvar uri_kinds
    | (KVar var as kvar) :: uri_kinds ->
      update_path
        ~f:(function
          | KVar var', _ -> String.equal var.name var'.name
          | _ -> false)
        t kvar uri_kinds
  and update_path ~f t kvar uri_kinds =
    (Queue.find t.path ~f
    |> function
    | Some (kvar, t') -> Queue.enqueue t.path (kvar, loop t' uri_kinds)
    | None -> Queue.enqueue t.path (kvar, loop empty uri_kinds));
    t
  in
  loop t (uri_kind uri)

type decoded_value = D : 'c var * 'c -> decoded_value

let rec match' : 'b t -> string -> 'b option =
 fun t uri ->
  let rec loop t decoded_values uri_tokens =
    match uri_tokens with
    | [] ->
      Option.map t.route ~f:(fun (Route (uri, f)) ->
          exec_route_handler f (uri, List.rev decoded_values))
    | uri_token :: uri_tokens ->
      Queue.fold_until t.path ~init:None
        ~f:
          (fun _ -> function
            | KVar var, t' -> (
              match var.decode uri_token with
              | Some v -> Stop (Some (D (var, v) :: decoded_values, t'))
              | None -> Continue None)
            | KLiteral lit, t' ->
              if String.equal lit uri_token then
                Stop (Some (decoded_values, t'))
              else
                Continue None)
        ~finish:(fun _ -> None)
      |> Option.bind ~f:(fun (decoded_values, t') ->
             (loop [@tailcall]) t' decoded_values uri_tokens)
  in
  let uri_tokens =
    String.rstrip uri ~drop:(function
      | '/' -> true
      | _ -> false)
    |> String.split ~on:'/'
  in
  let uri_tokens = List.slice uri_tokens 1 (List.length uri_tokens) in
  loop t [] uri_tokens

and exec_route_handler : type a b. a -> (a, b) uri * decoded_value list -> b =
 fun f -> function
  | End, [] -> f
  | Literal (_, uri), decoded_values ->
    exec_route_handler f (uri, decoded_values)
  | Var ({ tid; _ }, uri), D ({ tid = tid'; _ }, v) :: decoded_values -> (
    match Var_ty.eq tid tid' with
    | Some Var_ty.Eq -> exec_route_handler (f v) (uri, decoded_values)
    | None -> assert false)
  | _, _ -> assert false

let r1 = string (int end_) >- fun (s : string) (i : int) -> s ^ string_of_int i

let r2 = lit "home" (lit "about" end_) >- ""

let r3 = lit "home" (int end_) >- fun (i : int) -> string_of_int i

let r4 = lit "home" (float end_) >- fun (f : float) -> string_of_float f

let router = empty |> add r1 |> add r2 |> add r3 |> add r4

let _m = match' router "/home/100001"

(** This should give error (we added an extra () var in handler) but it doesn't.
    It only errors when adding to the router.*)
(* let r5 = *)
(* string (int end_) @-> fun (s : string) (i : int) () -> s ^ string_of_int i *)

(* |> add r5  *)
(* This errors *)
