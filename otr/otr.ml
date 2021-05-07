(** Variable type. *)
module Var_ty = struct
  type _ t = ..

  type _ t += Int : int t | Float : float t | Bool : bool t | String : string t

  type (_, _) eq = Eq : ('a, 'a) eq

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun aty bty ->
    match (aty, bty) with
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
  { name : string (* name e.g. int, float, bool, string etc *)
  ; decode : string -> 'c option
  ; ty : 'c Var_ty.t
  }

let end_ : ('b, 'b) uri = End

let lit : string -> ('a, 'b) uri -> ('a, 'b) uri = fun s uri -> Literal (s, uri)

let var decode name ty uri = Var ({ decode; name; ty }, uri)

let string : ('a, 'b) uri -> (string -> 'a, 'b) uri =
 fun uri -> var (fun s -> Some s) "string" Var_ty.String uri

let int : ('a, 'b) uri -> (int -> 'a, 'b) uri =
 fun uri -> var int_of_string_opt "int" Var_ty.Int uri

let float : ('a, 'b) uri -> (float -> 'a, 'b) uri =
 fun uri -> var float_of_string_opt "float" Var_ty.Float uri

let bool : ('a, 'b) uri -> (bool -> 'a, 'b) uri =
 fun uri -> var bool_of_string_opt "bool" Var_ty.Bool uri

(** ['c route] is a uri and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route = Route : ('a, 'c) uri * 'a -> 'c route

(** [p >- route_handler] creates a route from uri [p] and [route_handler]. *)
let ( >- ) : ('a, 'b) uri -> 'a -> 'b route = fun uri f -> Route (uri, f)

(** [uri_kind] Existential which encodes uri kind/type. *)
module Uri_kind = struct
  type t =
    | KLiteral : string -> t
    | KVar : 'c var -> t

  let equal a b =
    match (a, b) with
    | KLiteral lit', KLiteral lit -> String.equal lit lit'
    | KVar var', KVar var -> (
      match Var_ty.eq var.ty var'.ty with
      | Some Var_ty.Eq -> true
      | None -> false)
    | _ -> false

  (* [of_uri uri] converts [uri] to [kind list]. This is done to get around OCaml
     type inference issue when using [uri] type in the [add] function below. *)
  let rec of_uri : type a b. (a, b) uri -> t list = function
    | End -> []
    | Literal (lit, uri) -> KLiteral lit :: of_uri uri
    | Var (var, uri) -> KVar var :: of_uri uri
end

(** ['a t] is a node in a trie based router. *)
type 'a t =
  { route : 'a route option
  ; path : (Uri_kind.t * 'a t) list
  }

let update_path t path = { t with path }

let empty : 'a t = { route = None; path = [] }

let add (Route (uri, _) as route) t =
  let rec loop t = function
    | [] -> { t with route = Some route }
    | uri_kind :: uri_kinds ->
      List.find_opt
        (fun (uri_kind', _) -> Uri_kind.equal uri_kind uri_kind')
        t.path
      |> (function
           | Some _ ->
             List.map
               (fun (uri_kind', t') ->
                 if Uri_kind.equal uri_kind uri_kind' then
                   (uri_kind', loop t' uri_kinds)
                 else
                   (uri_kind', t'))
               t.path
           | None -> (uri_kind, loop empty uri_kinds) :: t.path)
      |> update_path t
  in
  loop t (Uri_kind.of_uri uri)

type 'a t_compiled =
  { route : 'a route option
  ; path : (Uri_kind.t * 'a t_compiled) array
  }

let rec compile : 'a t -> 'a t_compiled =
 fun t ->
  { route = t.route
  ; path =
      List.rev t.path
      |> List.map (fun (uri_kind, t) -> (uri_kind, compile t))
      |> Array.of_list
  }

type decoded_value = D : 'c var * 'c -> decoded_value

let rec match' t uri =
  let rec loop t decoded_values = function
    | [] ->
      Option.map
        (fun (Route (uri, f)) ->
          exec_route_handler f (uri, List.rev decoded_values))
        t.route
    | uri_token :: uri_tokens ->
      let continue = ref true in
      let index = ref 0 in
      let matched_comp = ref None in
      while !continue && !index < Array.length t.path do
        let p = t.path.(!index) in
        match p with
        | KVar var, t' -> (
          match var.decode uri_token with
          | Some v ->
            matched_comp := Some (D (var, v) :: decoded_values, t');
            continue := false
          | None -> incr index)
        | KLiteral lit, t' when String.equal lit uri_token ->
          matched_comp := Some (decoded_values, t');
          continue := false
        | _ -> incr index
      done;
      Option.bind !matched_comp (fun (decoded_values, t') ->
          (loop [@tailcall]) t' decoded_values uri_tokens)
  in
  String.split_on_char '/' uri
  |> List.filter (fun tok -> not (String.equal "" tok))
  |> loop t []

and exec_route_handler : type a b. a -> (a, b) uri * decoded_value list -> b =
 fun f -> function
  | End, [] -> f
  | Literal (_, uri), decoded_values ->
    exec_route_handler f (uri, decoded_values)
  | Var ({ ty; _ }, uri), D ({ ty = ty'; _ }, v) :: decoded_values -> (
    match Var_ty.eq ty ty' with
    | Some Var_ty.Eq -> exec_route_handler (f v) (uri, decoded_values)
    | None -> assert false)
  | _, _ -> assert false

let r1 = string (int end_) >- fun s (i : int) -> s ^ string_of_int i

let r2 = lit "home" (lit "about" end_) >- "about"

let r3 = lit "home" (int end_) >- fun i -> "int " ^ string_of_int i

let r4 = lit "home" (float end_) >- fun f -> "float " ^ string_of_float f

let router = empty |> add r2 |> add r3 |> add r4 |> add r1

let router = compile router

let _m = match' router "/home/100001.1"

let _m1 = match' router "/home/100001"

let _m2 = match' router "/home/about"

(** This should give error (we added an extra () var in handler) but it doesn't.
    It only errors when adding to the router.*)
(* let r5 = *)
(* string (int end_) @-> fun (s : string) (i : int) () -> s ^ string_of_int i *)

(* |> add r5  *)
(* This errors *)