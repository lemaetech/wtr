(** Variable type - a type witness for ['c var] below. *)
module Ty = struct
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

type 'a ty = 'a Ty.t = ..

include Ty

(** [('a, 'b) path] represents a uniform resource identifier. The variant
    members describe the path path component types. *)
type ('a, 'b) path =
  | End : ('b, 'b) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
      (** Uri literal string component eg. 'home' in '/home' *)
  | Var : 'c var * ('a, 'b) path -> ('c -> 'a, 'b) path
      (** Uri variable component, i.e. the value is determined dpathng runtime,
          eg. ':int' in '/home/:int' *)

and 'c var =
  { name : string (* name e.g. int, float, bool, string etc *)
  ; decode : string -> 'c option
  ; ty : 'c Ty.t
  }

let end_ : ('b, 'b) path = End

let lit : string -> ('a, 'b) path -> ('a, 'b) path =
 fun s path -> Literal (s, path)

let var decode name ty path = Var ({ decode; name; ty }, path)

let string : ('a, 'b) path -> (string -> 'a, 'b) path =
 fun path -> var (fun s -> Some s) "string" Ty.String path

let int : ('a, 'b) path -> (int -> 'a, 'b) path =
 fun path -> var int_of_string_opt "int" Ty.Int path

let float : ('a, 'b) path -> (float -> 'a, 'b) path =
 fun path -> var float_of_string_opt "float" Ty.Float path

let bool : ('a, 'b) path -> (bool -> 'a, 'b) path =
 fun path -> var bool_of_string_opt "bool" Ty.Bool path

type 'c route = Route : ('a, 'c) path * 'a -> 'c route

let ( >- ) : ('a, 'b) path -> 'a -> 'b route = fun path f -> Route (path, f)

(** [path_kind] Existential which encodes path kind/type. *)
module Uri_kind = struct
  type t =
    | KLiteral : string -> t
    | KVar : 'c var -> t

  let equal a b =
    match (a, b) with
    | KLiteral lit', KLiteral lit -> String.equal lit lit'
    | KVar var', KVar var -> (
      match Ty.eq var.ty var'.ty with
      | Some Ty.Eq -> true
      | None -> false)
    | _ -> false

  (* [of_path path] converts [path] to [kind list]. This is done to get around OCaml
     type inference issue when using [path] type in the [add] function below. *)
  let rec of_path : type a b. (a, b) path -> t list = function
    | End -> []
    | Literal (lit, path) -> KLiteral lit :: of_path path
    | Var (var, path) -> KVar var :: of_path path
end

(** ['a t] is a node in a trie based router. *)
type 'a node =
  { route : 'a route option
  ; path : (Uri_kind.t * 'a node) list
  }

let update_path t path = { t with path }

let empty : 'a node = { route = None; path = [] }

let add t (Route (path, _) as route) =
  let rec loop t = function
    | [] -> { t with route = Some route }
    | path_kind :: path_kinds ->
      List.find_opt
        (fun (path_kind', _) -> Uri_kind.equal path_kind path_kind')
        t.path
      |> (function
           | Some _ ->
             List.map
               (fun (path_kind', t') ->
                 if Uri_kind.equal path_kind path_kind' then
                   (path_kind', loop t' path_kinds)
                 else
                   (path_kind', t'))
               t.path
           | None -> (path_kind, loop empty path_kinds) :: t.path)
      |> update_path t
  in
  loop t (Uri_kind.of_path path)

type 'a t =
  { route : 'a route option
  ; path : (Uri_kind.t * 'a t) array
  }

let rec create routes = List.fold_left add empty routes |> compile

and compile : 'a node -> 'a t =
 fun t ->
  { route = t.route
  ; path =
      List.rev t.path
      |> List.map (fun (path_kind, t) -> (path_kind, compile t))
      |> Array.of_list
  }

type decoded_value = D : 'c var * 'c -> decoded_value

let rec match' t path =
  let rec loop t decoded_values = function
    | [] ->
      Option.map
        (fun (Route (path, f)) ->
          exec_route_handler f (path, List.rev decoded_values))
        t.route
    | path_token :: path_tokens ->
      let continue = ref true in
      let index = ref 0 in
      let matched_comp = ref None in
      while !continue && !index < Array.length t.path do
        let p = t.path.(!index) in
        match p with
        | KVar var, t' -> (
          match var.decode path_token with
          | Some v ->
            matched_comp := Some (D (var, v) :: decoded_values, t');
            continue := false
          | None -> incr index)
        | KLiteral lit, t' when String.equal lit path_token ->
          matched_comp := Some (decoded_values, t');
          continue := false
        | _ -> incr index
      done;
      Option.bind !matched_comp (fun (decoded_values, t') ->
          (loop [@tailcall]) t' decoded_values path_tokens)
  in
  String.split_on_char '/' path
  |> List.filter (fun tok -> not (String.equal "" tok))
  |> loop t []

and exec_route_handler : type a b. a -> (a, b) path * decoded_value list -> b =
 fun f -> function
  | End, [] -> f
  | Literal (_, path), decoded_values ->
    exec_route_handler f (path, decoded_values)
  | Var ({ ty; _ }, path), D ({ ty = ty'; _ }, v) :: decoded_values -> (
    match Ty.eq ty ty' with
    | Some Ty.Eq -> exec_route_handler (f v) (path, decoded_values)
    | None -> assert false)
  | _, _ -> assert false

let r1 = string (int end_) >- fun s (i : int) -> s ^ string_of_int i

let r2 = lit "home" (lit "about" end_) >- "about"

let r3 = lit "home" (int end_) >- fun i -> "int " ^ string_of_int i

let r4 = lit "home" (float end_) >- fun f -> "float " ^ string_of_float f

let router = create [ r2; r3; r4; r1 ]

let _m = match' router "/home/100001.1"

let _m1 = match' router "/home/100001"

let _m2 = match' router "/home/about"
